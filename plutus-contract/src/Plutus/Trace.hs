{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Plutus.Trace where

import Ledger.Slot (Slot)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Extras
import qualified Data.Aeson as JSON
import Wallet.Emulator.Wallet (Wallet(..))
import Wallet.Types (ContractInstanceId)
import Control.Monad.Freer.Coroutine
import Data.Void (Void)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity)
import Language.Plutus.Contract (Contract, HasEndpoint)

class SimulatorBackend a where
  type LocalAction a :: * -> *
  type GlobalAction a :: * -> *
  type Agent a

data Simulator a b where
    RunLocal :: SimulatorBackend a => Agent a -> LocalAction a b -> Simulator a b
    RunGlobal :: SimulatorBackend a => GlobalAction a b -> Simulator a b

data Playground

data PlaygroundLocal r where
   CallEndpoint :: String -> JSON.Value -> PlaygroundLocal ()

data PlaygroundGlobal r where
   WaitForSlot :: Slot -> PlaygroundGlobal ()  

instance SimulatorBackend Playground where
    type LocalAction Playground = PlaygroundLocal
    type GlobalAction Playground = PlaygroundGlobal
    type Agent Playground = Wallet

-- | Playground traces need to be serialisable, so they are just
--   lists of single 'PlaygroundAction's.
type PlaygroundAction = Simulator Playground ()
type PlaygroundTrace = [PlaygroundAction]

ptrace :: PlaygroundTrace
ptrace = 
    [ RunLocal (Wallet 1) $ CallEndpoint "submit" (JSON.toJSON "100 Ada")
    , RunGlobal $ WaitForSlot 10
    ]

data Emulator

-- | A reference to an installed contract in the emulator.
newtype ContractHandle s e = ContractHandle { unContractHandle :: Contract s e () }

-- | A reference to a running contract in the emulator.
type RunningContract s e = Const ContractInstanceId (Contract s e ())

data EmulatorLocal r where
    InstallContract :: Contract s e () -> EmulatorLocal (ContractHandle s e)
    ActivateContract :: ContractHandle s e -> EmulatorLocal (RunningContract s e)
    CallEndpointEm :: forall l ep s e. HasEndpoint l ep s => RunningContract s e -> ep -> EmulatorLocal ()

data EmulatorGlobal r where

instance SimulatorBackend Emulator where
    type LocalAction Emulator = EmulatorLocal
    type GlobalAction Emulator = EmulatorGlobal

type EmulatorTrace = Eff '[Simulator Emulator] ()

data ContinueWhen
    = ThisSlot -- ^ Yield control (cooperative multitasking), resuming computation in the same slot
    | NextSlot (Maybe Slot) -- ^ Sleep until the given slot

data EmThreadId a =
    EmContractInstance ContractInstanceId
    | EmUser (Agent a)
    | EmGlobalThread

newtype EmThread a effs = EmThread { emContinuation :: Eff (Simulator a ': effs) () }

data SuspendedThread a effs =
    SuspendedThread
        { stWhen :: ContinueWhen
        , stThread :: EmThread a effs
        }

data SchedulerState a effs =
    SchedulerState
        { currentSlotActions :: [EmThread a effs]
        , futureActions :: [EmThread a effs]
        , currentSlot :: Slot
        }

initialState :: SchedulerState a effs
initialState = SchedulerState [] [] 0

enqueueThisSlot :: EmThread a effs -> SchedulerState a effs -> SchedulerState a effs
enqueueThisSlot thread s =
    s{currentSlotActions = currentSlotActions s ++ [thread]}

enqueueNextSlot :: EmThread a effs -> SchedulerState a effs -> SchedulerState a effs
enqueueNextSlot thread s =
    s{futureActions=futureActions s ++ [thread]}

increaseSlot :: SchedulerState a effs -> SchedulerState a effs
increaseSlot s@SchedulerState{currentSlot} = s {currentSlot = currentSlot + 1}

data SimulatorInterpreter a effs =
    SimulatorInterpreter
        { interpRunLocal :: forall b. Agent a -> LocalAction a b -> Eff effs (b, EmThread a effs)
        , interpRunGlobal :: forall b. GlobalAction a b -> Eff effs (b, EmThread a effs)
        }

newtype GlobalThreadId = GlobalThreadId Int
newtype ContractThreadId = ContractThreadId ContractThreadId

type YieldGobal effs = Yield (Eff effs ()) GlobalThreadId
type YieldContract effs = Yield (Eff effs ()) ContractThreadId

type SchedulerEffs a effs = Yield (SuspendedThread a effs) () ': effs

handleEmulator :: forall a effs.
    SimulatorInterpreter a effs
    -> Simulator a
    ~> Eff (SchedulerEffs a effs)
handleEmulator SimulatorInterpreter{interpRunGlobal, interpRunLocal} = \case
    RunLocal wllt localAction -> do
        (b, thread) <- raise $ interpRunLocal wllt localAction
        let t = SuspendedThread{stWhen = ThisSlot, stThread=thread}
        _ <- yield @(SuspendedThread a effs) @() t id
        pure b
    RunGlobal globalAction -> do
        (b, thread) <- raise $ interpRunGlobal globalAction
        let t = SuspendedThread{stWhen = ThisSlot, stThread=thread}
        _ <- yield @(SuspendedThread a effs) @() t id
        pure b

runScheduler :: forall a effs.
    SimulatorInterpreter a effs
    -> Eff (Yield (SuspendedThread a effs) () ': effs) ()
    -> Eff effs ()
runScheduler i e = loop [runC e] where
    loop :: [Eff effs (Status effs (SuspendedThread a effs) () ())] -> Eff effs ()
    loop readyQ = case readyQ of
        [] -> pure ()
        (x:xs) -> do
            result <- x
            case result of
                Done _ -> loop xs
                Continue (SuspendedThread{stWhen, stThread=EmThread t}) k -> do
                    let thread' = runC $ interpret (handleEmulator i)  (raiseUnder @effs @(Simulator a) t)
                    loop (xs ++ [thread', k ()])
