{-# LANGUAGE TypeFamilies          #-}
-- | The CEK machine.
-- Rules are the same as for the CK machine except we do not use substitution and use
-- environments instead.
-- The CEK machine relies on variables having non-equal 'Unique's whenever they have non-equal
-- string names. I.e. 'Unique's are used instead of string names. This is for efficiency reasons.
-- The CEK machines handles name capture by design.
-- The type checker pass is a prerequisite.
-- Feeding ill-typed terms to the CEK machine will likely result in a 'MachineException'.
-- Dynamic extensions to the set of built-ins are allowed.
-- In case an unknown dynamic built-in is encountered, an 'UnknownDynamicBuiltinNameError' is returned
-- (wrapped in 'OtherMachineError').

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PlutusCore.Evaluation.Machine.Cek
    ( Val(..)
    , EvaluationResult(..)
    , ErrorWithCause(..)
    , MachineError(..)
    , CekMachineException
    , EvaluationError(..)
    , CekUserError(..)
    , CekEvaluationException
    , ExBudgetState(..)
    , ExTally(..)
    , ExBudget(..)
    , ExRestrictingBudget(..)
    , ExBudgetMode(..)
    , Plain
    , WithMemory
    , extractEvaluationResult
    , cekEnvMeans
    , cekEnvVarEnv
    , exBudgetStateTally
    , exBudgetStateBudget
    , exBudgetCPU
    , exBudgetMemory
    , runCek
    , runCekCounting
    , evaluateCek
    , unsafeEvaluateCek
    , readKnownCek
    )
where

import           PlutusPrelude

import           Language.PlutusCore.Constant
import           Language.PlutusCore.Core
import           Language.PlutusCore.Error
import           Language.PlutusCore.Evaluation.Machine.ExBudgeting
import           Language.PlutusCore.Evaluation.Machine.Exception
import           Language.PlutusCore.Evaluation.Machine.ExMemory
import           Language.PlutusCore.Evaluation.Result
import           Language.PlutusCore.MkPlc                          hiding (error)
import           Language.PlutusCore.Name
import           Language.PlutusCore.Pretty
import           Language.PlutusCore.Universe
import           Language.PlutusCore.View

import           Control.Lens.Operators
import           Control.Lens.Setter
import           Control.Lens.TH                                    (makeLenses)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.HashMap.Monoidal
import qualified Data.Map                                           as Map
import           Data.Text.Prettyprint.Doc

import           Data.Array

deriving instance Ix BuiltinName

builtinNameArities :: Array BuiltinName Int
builtinNameArities =
    listArray (minBound, maxBound) $
        [minBound..maxBound] <&> \name ->
            withTypedBuiltinName @_ @(Term TyName Name DefaultUni ()) name $
                \(TypedBuiltinName _ sch) -> countArgs sch
{-# NOINLINE builtinNameArities #-}  -- Just in case.

{- Note [Scoping]
The CEK machine does not rely on the global uniqueness condition, so the renamer pass is not a
prerequisite. The CEK machine correctly handles name shadowing.
-}

type TermWithMem uni = WithMemory Term uni
type TypeWithMem uni = Type TyName uni ExMemory
type KindWithMem = Kind ExMemory

-- 'Values' for the modified CEK machine.
data Val uni =
    -- TODO: we probably want to store a @Some (ValueOf uni)@ here, but then we have trouble in
    -- 'readKnownCek'. I'll reconsider the way we deal with annotations once again.
    VCon (TermWithMem uni)
  | VTyAbs ExMemory TyName KindWithMem (TermWithMem uni) (ValEnv uni)
  | VLamAbs ExMemory Name (TypeWithMem uni) (TermWithMem uni) (ValEnv uni)
  | VIWrap ExMemory (TypeWithMem uni) (TypeWithMem uni) (Val uni)
  | VBuiltin
      ExMemory
      (ValEnv uni)  -- Initial environment, used for evaluating every argument
      StagedBuiltinName
      Int           -- Number of arguments to be provided
      [TypeWithMem uni] -- The types the builtin is to be instantiated at.  We don't really need these.
      [Val uni] -- Arguments we've computed so far.
    deriving (Show, Eq)  -- Eq is just for tests.

instance PrettyBy PrettyConfigPlc (Val uni) where
    prettyBy _ _ = "<val>"

type instance UniOf (Val uni) = uni

instance (Closed uni, uni `Everywhere` ExMemoryUsage) => HasConstant (Val uni) where
    fromConstant = VCon . fromConstant

    asConstant (VCon term) = asConstant term
    asConstant _           = Nothing

valEx :: Val uni -> ExMemory
valEx = \case
    VCon t -> termAnn t
    VTyAbs ex _ _ _ _ -> ex
    VLamAbs ex _ _ _ _ -> ex
    VIWrap ex _ _ _ -> ex
    VBuiltin ex _ _ _ _ _ -> ex

-- TODO: this doesn't discharge the environments.
dischargeVal :: Val uni -> WithMemory Term uni
dischargeVal = \case
    VCon t -> t
    VTyAbs ex tn k body _env -> TyAbs ex tn k body
    VLamAbs ex name ty body _env -> LamAbs ex name ty body
    VIWrap ex ty1 ty2 val -> IWrap ex ty1 ty2 $ dischargeVal val
    VBuiltin{} -> error "Discharging VBuiltin"
    -- We'll only get this with a stuck partial application - should really make it back into a proper term


type ValEnv uni = UniqueMap TermUnique (Val uni)

data CekUserError
    = CekOutOfExError ExRestrictingBudget ExBudget
    | CekEvaluationFailure -- ^ Error has been called.
    deriving (Show, Eq)

-- | The CEK machine-specific 'MachineException'.
type CekMachineException uni =
    MachineException UnknownDynamicBuiltinNameError (Val uni)

-- | The CEK machine-specific 'EvaluationException'.
type CekEvaluationException uni =
    EvaluationException UnknownDynamicBuiltinNameError CekUserError (Val uni)

instance Pretty CekUserError where
    pretty (CekOutOfExError (ExRestrictingBudget res) b) =
        group $ "The limit" <+> prettyClassicDef res <+> "was reached by the execution environment. Final state:" <+> prettyClassicDef b
    pretty CekEvaluationFailure = "The provided Plutus code called 'error'."

-- | The environment the CEK machine runs in.
data CekEnv uni = CekEnv
    { _cekEnvMeans             :: DynamicBuiltinNameMeanings (Val uni)
    , _cekEnvVarEnv            :: ValEnv uni
    , _cekEnvBudgetMode        :: ExBudgetMode
    , _cekEnvBuiltinCostParams :: CostModel
    }

makeLenses ''CekEnv

-- | The monad the CEK machine runs in. State is inside the ExceptT, so we can
-- get it back in case of error.
type CekM uni = ReaderT (CekEnv uni) (ExceptT (CekEvaluationException uni) (State ExBudgetState))

instance SpendBudget (CekM uni) (Val uni) where
    builtinCostParams = view cekEnvBuiltinCostParams
    getExMemory = pure . valEx
    spendBudget key budget = do
        modifying exBudgetStateTally
                (<> (ExTally (singleton key budget)))
        newBudget <- exBudgetStateBudget <%= (<> budget)
        mode <- view cekEnvBudgetMode
        case mode of
            Counting -> pure ()
            Restricting resb ->
                when (exceedsBudget resb newBudget) $
                    throwingWithCause _EvaluationError
                        (UserEvaluationError $ CekOutOfExError resb newBudget)
                        Nothing

{-
 f := [_ (M,ρ) ]
   | [V _]
   | {_ A}
   | wrap A B _
   | unwrap _
   | builtin b As Vs _ Ms ρ
-}

data Frame uni
    = FrameApplyFun (Val uni)                                                    -- ^ @[V _]@
    | FrameApplyArg (ValEnv uni) (WithMemory Term uni)                           -- ^ @[_ N]@
    | FrameTyInstArg (Type TyName uni ExMemory)                                  -- ^ @{_ A}@
    | FrameUnwrap                                                                -- ^ @(unwrap _)@
    | FrameIWrap ExMemory (Type TyName uni ExMemory) (Type TyName uni ExMemory)  -- ^ @(iwrap A B _)@
 deriving (Show)

type Context uni = [Frame uni]

runCekM
    :: forall a uni
     . CekEnv uni
    -> ExBudgetState
    -> CekM uni a
    -> (Either (CekEvaluationException uni) a, ExBudgetState)
runCekM env s a = runState (runExceptT $ runReaderT a env) s

-- | Get the current 'ValEnv'.
getEnv :: CekM uni (ValEnv uni)
getEnv = asks _cekEnvVarEnv

-- | Set a new 'VarEnv' and proceed.
withEnv :: ValEnv uni -> CekM uni a -> CekM uni a
withEnv venv = local (set cekEnvVarEnv venv)

-- | Extend an environment with a variable name, the value the variable stands for
-- and the environment the value is defined in.
extendEnv :: Name -> Val uni -> ValEnv uni -> ValEnv uni
extendEnv argName arg  =
    insertByName argName arg

-- | Look up a variable name in the environment.
lookupVarName :: Name -> CekM uni (Val uni)
lookupVarName varName = do
    varEnv <- getEnv
    case lookupName varName varEnv of
        Nothing   -> throwingWithCause _MachineError
            OpenTermEvaluatedMachineError
            Nothing -- (Just . Var (memoryUsage ()) $ varName)
        Just val -> pure val

-- | Look up a 'DynamicBuiltinName' in the environment.
lookupDynamicBuiltinName
    :: DynamicBuiltinName -> CekM uni (DynamicBuiltinNameMeaning (Val uni))
lookupDynamicBuiltinName dynName = do
    DynamicBuiltinNameMeanings means <- asks _cekEnvMeans
    case Map.lookup dynName means of
        Nothing   -> throwingWithCause _MachineError err Nothing where -- $ Just term where
            err  = OtherMachineError $ UnknownDynamicBuiltinNameErrorE dynName
--             term = Builtin (memoryUsage ()) $ DynBuiltinName (memoryUsage ()) dynName
        Just mean -> pure mean

{- Note [Dropping environments of arguments]
The CEK machine sometimes keeps in the environment those variables that are no longer required.
This is a fundamental limitation of the CEK machine as it lacks garbage collection.
There are alternative machines that implement some form of environment cleaning (CESK, for example).
But if we're going to expore this space, it's better to jump straight to something close to actual
hardware than to deal with inherently inefficient abstract machines.

But if we could optimize the current evaluator at small development/maintenance cost, that would be
useful. One such opportunity is to drop the environment of a constant as a constant can't reference
any variables. So we do that in this line:

    computeCek con constant@Constant{} = withVarEnv mempty $ returnCek con constant

We can't drop the environment of a built-in function, because it can be polymorphic and thus can
receive arbitrary terms as arguments.

A 'TyAbs'- or 'LamAbs'-headed term may also reference free variables.

In the 'Var' case we drop the current environment, look up the variable and use the environment
stored in the looked up closure as per the normal control flow of the CEK machine.

Note that if we had polymorphic built-in types, we couldn't drop the environment of a constant as,
say, a `list nat` can contain arbitrary terms of type `nat` and thus can reference variables from
the environment. Polymorphic built-in types complicate evaluation in general as unlifting, say,
a `list integer` constant may require evaluating terms *inside* the constant (particularly, if that
constant was constructed by a built-in function). Similar complications associated with looking into
constants of polymorphic built-in types arise for other procedures (pretty-printing, type checking,
substitution, anything).
-}

-- See Note [Dropping environments of arguments].
-- | The computing part of the CEK machine.
-- Either
-- 1. adds a frame to the context and calls 'computeCek' ('TyInst', 'Apply', 'IWrap', 'Unwrap')
-- 2. calls 'returnCek' on values ('TyAbs', 'LamAbs', 'Constant')
-- 3. returns 'EvaluationFailure' ('Error')
-- 4. looks up a variable in the environment and calls 'returnCek' ('Var')



getArgsCount :: Builtin ann -> CekM uni Int
getArgsCount (BuiltinName _ name) =
    pure $ builtinNameArities ! name
getArgsCount (DynBuiltinName _ name) = do
    DynamicBuiltinNameMeaning sch _ _ <- lookupDynamicBuiltinName name
    pure $ countArgs sch

computeCek
    :: (GShow uni, GEq uni, DefaultUni <: uni, Closed uni, uni `Everywhere` ExMemoryUsage)
    => Context uni -> WithMemory Term uni -> CekM uni (Plain Term uni)
-- s ; ρ ▻ {L A}  ↦ s , {_ A} ; ρ ▻ L
computeCek ctx t@(TyInst _ body ty) = do
    spendBudget BTyInst (ExBudget 1 1) -- TODO
    computeCek (FrameTyInstArg ty : ctx) body
-- s ; ρ ▻ [L M]  ↦  s , [_ (M,ρ)]  ; ρ ▻ L
computeCek ctx t@(Apply _ fun arg) = do
    spendBudget BApply (ExBudget 1 1) -- TODO
    env <- getEnv
    computeCek (FrameApplyArg env arg : ctx) fun
-- s ; ρ ▻ wrap A B L  ↦  s , wrap A B _ ; ρ ▻ L
computeCek ctx t@(IWrap ann pat arg term) = do
    spendBudget BIWrap (ExBudget 1 1) -- TODO
    computeCek (FrameIWrap ann pat arg : ctx) term
-- s ; ρ ▻ unwrap L  ↦  s , unwrap _ ; ρ ▻ L
computeCek ctx t@(Unwrap _ term) = do
    spendBudget BUnwrap (ExBudget 1 1) -- TODO
    computeCek (FrameUnwrap : ctx) term
-- s ; ρ ▻ abs α L  ↦  s ◅ abs α (L , ρ)
computeCek ctx (TyAbs ex tn k body)  = do
    env <- getEnv
    returnCek ctx (VTyAbs ex tn k body env)
-- s ; ρ ▻ lam x L  ↦  s ◅ lam x (L , ρ)
computeCek ctx (LamAbs ex name ty body)  = do
    env <- getEnv
    returnCek ctx (VLamAbs ex name ty body env)
-- s ; ρ ▻ con c  ↦  s ◅ con c
computeCek ctx constant@Constant{} = returnCek ctx (VCon constant)
computeCek ctx (Builtin ex bn)        = do
  env <- getEnv
  count <- getArgsCount bn
  returnCek ctx (VBuiltin ex env (constantAsStagedBuiltinName bn) count [] [])
-- s ; ρ ▻ error A  ↦  <> A
computeCek _   err@Error{} =
    throwingWithCause _EvaluationError (UserEvaluationError CekEvaluationFailure) Nothing -- $ Just err
--  s ; ρ ▻ x  ↦  s ◅ ρ[ x ]
computeCek ctx t@(Var _ varName)   = do
    spendBudget BVar (ExBudget 1 1) -- TODO
    val <- lookupVarName varName
    returnCek ctx val

-- | The returning part of the CEK machine.
-- Returns 'EvaluationSuccess' in case the context is empty, otherwise pops up one frame
-- from the context and either
-- 1. performs reduction and calls 'computeCek' ('FrameTyInstArg', 'FrameApplyFun', 'FrameUnwrap')
-- 2. performs a constant application and calls 'returnCek' ('FrameTyInstArg', 'FrameApplyFun')
-- 3. puts 'FrameApplyFun' on top of the context and proceeds with the argument from 'FrameApplyArg'
-- 4. grows the resulting term ('FrameWrap')

-- s , builtin b As Vs _ [] ρ ◅ V           ↦
--  | bn computes on As (Vs ++ [V]) to V'      = s ◅ V'
--  | bn computes on As (VS ++ [V]) to error A = <> A
-- s , builtin b As Vs _ (M ∷ Ms) ρ ◅ V     ↦  s , builtin b As (Vs ++ [V]) _ Ms ρ ; ρ ▻ M

returnCek
    :: (GShow uni, GEq uni, DefaultUni <: uni, Closed uni, uni `Everywhere` ExMemoryUsage)
    => Context uni -> Val uni -> CekM uni (Plain Term uni)
--- Instantiate all the free variable of the resulting term in case there are any.
-- . ◅ V           ↦  [] V
returnCek [] val = pure $ void $ dischargeVal val
-- s , {_ A} ◅ abs α M  ↦  s ; ρ ▻ M [ α / A ]*
returnCek (FrameTyInstArg ty : ctx) fun = instantiateEvaluate ctx ty fun
-- s , [_ (M,ρ)] ◅ V  ↦  s , [V _] ; ρ ▻ M
returnCek (FrameApplyArg argVarEnv arg : ctx) fun = do
    -- funVarEnv <- getEnv
    withEnv argVarEnv $ computeCek (FrameApplyFun fun : ctx) arg
-- s , [(lam x (M,ρ)) _] ◅ V  ↦  s ; ρ [ x  ↦  V ] ▻ M
returnCek (FrameApplyFun fun : ctx) arg = do
    applyEvaluate ctx fun arg
-- s , wrap A B _ ◅ V  ↦  s ◅ wrap A B V
returnCek (FrameIWrap ex pat arg : ctx) val =
    returnCek ctx $ VIWrap ex pat arg val
-- s , unwrap _ ◅ wrap A B V  ↦  s ◅ V
returnCek (FrameUnwrap : ctx) val =
    case val of
      VIWrap _ _ _ v -> returnCek ctx v
      _              ->
        throwingWithCause _MachineError NonWrapUnwrappedMachineError $ Nothing -- Just (void val)

-- | Instantiate a term with a type and proceed.
-- In case of 'TyAbs' just ignore the type. Otherwise check if the term is an
-- iterated application of a 'BuiltinName' to a list of 'Value's and, if succesful,
-- apply the term to the type via 'TyInst'.
instantiateEvaluate
    :: (GShow uni, GEq uni, DefaultUni <: uni, Closed uni, uni `Everywhere` ExMemoryUsage)
    => Context uni -> Type TyName uni ExMemory -> Val uni -> CekM uni (Plain Term uni)
instantiateEvaluate ctx _ (VTyAbs _ _ _ body env) = withEnv env $ computeCek ctx body -- FIXME: env?
instantiateEvaluate ctx ty (VBuiltin ex argEnv bn count tyargs args) =
    case args of
      [] -> returnCek ctx $ VBuiltin ex argEnv bn count (ty:tyargs) args  -- The types will be the wrong way round, but we never use them.
      _  -> error "Builtin instantiation after term argument"
instantiateEvaluate _ _ val =
        throwingWithCause _MachineError NonPrimitiveInstantiationMachineError $ Just val


-- We're applying a builtin to a list of Vals.  As before, if we've
-- got a const, feed it directly to the builtin. Otherwise, extend the
-- environment with a new variable bound to the Val and feed that to
-- the builtin instead and continue.

-- | Apply a function to an argument and proceed.
-- If the function is a 'LamAbs', then extend the current environment with a new variable and proceed.
-- If the function is not a 'LamAbs', then 'Apply' it to the argument and view this
-- as an iterated application of a 'BuiltinName' to a list of 'Value's.
-- If succesful, proceed with either this same term or with the result of the computation
-- depending on whether 'BuiltinName' is saturated or not.
applyEvaluate
    :: (GShow uni, GEq uni, DefaultUni <: uni, Closed uni, uni `Everywhere` ExMemoryUsage)
    =>  -- ValEnv uni ->
       Context uni
    -> Val uni   -- lsh of application
    -> Val uni   -- rhs of application
    -> CekM uni (Plain Term uni)
applyEvaluate ctx (VLamAbs _ name _ty body env) arg =
    withEnv (extendEnv name arg env) $ computeCek ctx body
applyEvaluate ctx (VBuiltin ex argEnv bn count tyargs args) arg = withEnv argEnv $ do
    let args' = arg:args
        count' = count - 1
    if count' /= 0
        then returnCek ctx $ VBuiltin ex argEnv bn count' tyargs args'
        else do
            res <- applyStagedBuiltinName bn (reverse args')
            case res of
                EvaluationSuccess t -> returnCek ctx t  -- NOTE that this is 'returnCek' now.
                EvaluationFailure ->
                    throwingWithCause _EvaluationError (UserEvaluationError CekEvaluationFailure) Nothing -- $ Just err

-- | Apply a 'StagedBuiltinName' to a list of 'Value's.
applyStagedBuiltinName
    :: (GShow uni, GEq uni, DefaultUni <: uni, Closed uni, uni `Everywhere` ExMemoryUsage)
    => StagedBuiltinName
    -> [Val uni]
    -> CekM uni (EvaluationResult (Val uni))
applyStagedBuiltinName n@(DynamicStagedBuiltinName name) args = do
    DynamicBuiltinNameMeaning sch x exX <- lookupDynamicBuiltinName name
    applyTypeSchemed n sch x exX args
applyStagedBuiltinName (StaticStagedBuiltinName name) args =
    applyBuiltinName name args

-- | Evaluate a term using the CEK machine and keep track of costing.
runCek
    :: (GShow uni, GEq uni, DefaultUni <: uni, Closed uni, uni `Everywhere` ExMemoryUsage)
    => DynamicBuiltinNameMeanings (Val uni)
    -> ExBudgetMode
    -> CostModel
    -> Plain Term uni
    -> (Either (CekEvaluationException uni) (Plain Term uni), ExBudgetState)
runCek means mode params term =
    runCekM (CekEnv means mempty mode params)
            (ExBudgetState mempty mempty)
        $ do
            spendBudget BAST (ExBudget 0 (termAnn memTerm))
            computeCek [] memTerm
    where
        memTerm = withMemory term

-- | Evaluate a term using the CEK machine in the 'Counting' mode.
runCekCounting
    :: (GShow uni, GEq uni, DefaultUni <: uni, Closed uni, uni `Everywhere` ExMemoryUsage)
    => DynamicBuiltinNameMeanings (Val uni)
    -> CostModel
    -> Plain Term uni
    -> (Either (CekEvaluationException uni) (Plain Term uni), ExBudgetState)
runCekCounting means = runCek means Counting

-- | Evaluate a term using the CEK machine.
evaluateCek
    :: (GShow uni, GEq uni, DefaultUni <: uni, Closed uni, uni `Everywhere` ExMemoryUsage)
    => DynamicBuiltinNameMeanings (Val uni)
    -> CostModel
    -> Plain Term uni
    -> Either (CekEvaluationException uni) (Plain Term uni)
evaluateCek means params = fst . runCekCounting means params

-- | Evaluate a term using the CEK machine. May throw a 'CekMachineException'.
unsafeEvaluateCek
    :: ( GShow uni, GEq uni, DefaultUni <: uni, Closed uni, uni `Everywhere` ExMemoryUsage
       , Typeable uni, uni `Everywhere` PrettyConst
       )
    => DynamicBuiltinNameMeanings (Val uni)
    -> CostModel
    -> Plain Term uni
    -> EvaluationResult (Plain Term uni)
unsafeEvaluateCek means params = either throw id . extractEvaluationResult . evaluateCek means params

-- | Unlift a value using the CEK machine.
readKnownCek
    :: ( GShow uni, GEq uni, DefaultUni <: uni, Closed uni, uni `Everywhere` ExMemoryUsage
       , KnownType (Plain Term uni) a
       )
    => DynamicBuiltinNameMeanings (Val uni)
    -> CostModel
    -> Plain Term uni
    -> Either (CekEvaluationException uni) a
-- Calling 'withMemory' just to unify the monads that 'readKnown' and the CEK machine run in.
readKnownCek means params = evaluateCek means params >=> first (fmap $ VCon . withMemory) . readKnown

{- Note [Saved mapping example]
Consider a polymorphic built-in function @id@, whose type signature on the Plutus side is

    id : all a. a -> a

Notation:

- the variable environment is denoted as @{ <var> :-> (<env>, <value>), ... }@
- the context is denoted as a Haskell list of 'Frame's
- 'computeCek' is denoted as @(▻)@
- 'returnCek' is denoted as @(◅)@

When evaluating

    id {integer -> integer} ((\(i : integer) (j : integer) -> i) 1) 0

We encounter the following state:

    { i :-> ({}, 1) }
    [ FrameApplyFun {} (id {integer -> integer})
    , FrameApplyArg {} 0
    ] ▻ \(j : integer) -> i

and transition it into

    { arg :-> ({ i :-> ({}, 1) }, \(j : integer) -> i) }
    [ FrameApplyArg {} 0
    ] ◅ id {integer -> integer} arg

i.e. if the argument is not a constant, then we create a new variable, save the old environment in
the closure of that variable and apply the function to the variable. This allows to restore the old
environment @{ i :-> ({}, 1) }@ latter when we start evaluating @arg 0@, which expands to

    (\(j : integer) -> i)) 0

which evaluates to @1@ in the old environment.
-}

-- See Note [Saved mapping example].
-- See https://github.com/input-output-hk/plutus/issues/1882 for discussion
-- | If an argument to a built-in function is a constant, then feed it directly to the continuation
-- that handles the argument and invoke the continuation in the caller's environment.
-- Otherwise create a fresh variable, save the environment of the argument in a closure, feed
-- the created variable to the continuation and invoke the continuation in the caller's environment
-- extended with a mapping from the created variable to the closure (i.e. original argument +
-- its environment). The "otherwise" is only supposed to happen when handling an argument to a
-- polymorphic built-in function.
