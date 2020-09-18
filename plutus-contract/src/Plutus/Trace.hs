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

data EmThread a effs =
    EmThread
        { emContinuation :: Slot -> Eff effs (Status effs (EmThreadId a, ContinueWhen) Slot ())
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

data SimulatorInterpreter a effs =
    SimulatorInterpreter
        { interpRunLocal :: forall b. Agent a -> LocalAction a b -> Eff (Yield (EmThreadId a, ContinueWhen) Slot ': effs) b
        , interpRunGlobal :: forall b. GlobalAction a b -> Eff (Yield (EmThreadId a, ContinueWhen) Slot ': effs) b
        }

-- | Evaluate the 'Simulator' actions, populating the 'SchedulerState' with
--   threads.
initialiseScheduler :: forall a effs.
    SimulatorInterpreter a effs
    -> Eff '[Simulator a]
    ~> Eff (Yield (EmThreadId a, ContinueWhen) Slot ': effs)
initialiseScheduler i = interpret (handleEmulator i) . raiseEnd

handleEmulator :: forall a effs.
    SimulatorInterpreter a effs
    -> Simulator a
    ~> Eff (Yield (EmThreadId a, ContinueWhen) Slot ': effs)
handleEmulator SimulatorInterpreter{interpRunGlobal, interpRunLocal} = \case
    RunLocal wllt localAction -> do
        -- be nice and go to the back of the queue
        _ <- yield @(EmThreadId a, ContinueWhen) @Slot (EmUser wllt, ThisSlot) id
        interpRunLocal wllt localAction
    RunGlobal globalAction -> do
        _ <- yield @(EmThreadId a, ContinueWhen) @Slot (EmGlobalThread, ThisSlot) id
        interpRunGlobal globalAction

runScheduler :: forall a effs.
    (Slot -> Eff effs ())
    -> Eff (Yield (EmThreadId a, ContinueWhen) Slot ': effs)
    ~> Eff effs
runScheduler onNextSlot e = runC e >>= loop initialState where

    loop :: forall x. SchedulerState a effs -> Status effs (EmThreadId a, ContinueWhen) Slot x -> Eff effs x
    loop state = \case
        Done a ->
            -- advance the clock, etc.
            pure a
        Continue (threadid, waitUntil) k -> do
            case waitUntil of
                    ThisSlot -> do
                        result' <- k (currentSlot state)
                        let state' = enqueueThisSlot EmThread{emContinuation = k } state
                        loop state' _
            undefined

-- | Run the threads round-robin style, advancing the clock whenever there is
--   nothing left to do in the current slot.
-- runScheduler :: forall a effs.
--     (Slot -> Eff effs ()) -- ^ What to do when a new slot starts
--     -> SchedulerState a effs -- ^ The scheduler
--     -> Eff effs ()
-- runScheduler onNextSlot s@SchedulerState{currentSlotActions, futureActions, currentSlot} = do
--     case currentSlotActions of
--         t@EmThread{emContinuation}:xs -> do
--             x' <- emContinuation currentSlot
--             let newState = case x' of
--                     Done () -> s{currentSlotActions = xs, futureActions = futureActions}
--                     Continue ThisSlot r -> s{currentSlotActions = xs ++ [t{emContinuation=r}], futureActions = futureActions}
--                     Continue (NextSlot _) r -> s{currentSlotActions = xs, futureActions = futureActions ++ [t{emContinuation=r}]}
--             runScheduler onNextSlot  newState
--         [] -> do
--              -- current slot is finished
--              let newSlot = currentSlot + 1
--                  newState = SchedulerState{currentSlotActions=futureActions, futureActions=[], currentSlot=newSlot}
--              onNextSlot newSlot
--              runScheduler onNextSlot newState

