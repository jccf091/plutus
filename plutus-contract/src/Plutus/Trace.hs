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

data EmThreadId =
    EmContractInstance ContractInstanceId
    | EmUser Wallet
    | EMGlobalThread

data EmThread effs =
    EmThread
        { emThreadID :: EmThreadId
        , emContinuation :: Slot -> Eff effs (Status effs ContinueWhen Slot ())
        }

data SchedulerState effs =
    SchedulerState
        { currentSlotActions :: [EmThread effs]
        , futureActions :: [EmThread effs]
        , currentSlot :: Slot
        }

initialState :: SchedulerState effs
initialState = SchedulerState [] [] 0

-- | Evaluate the 'Simulator' actions, populating the 'SchedulerState' with
--   threads.
initialiseScheduler :: forall effs.
    Eff '[Simulator Emulator] ()
    -> SchedulerState effs
initialiseScheduler =
    run
    . execState initialState
    . interpret handleEmulator
    . raiseEnd @'[State (SchedulerState effs)] @(Simulator Emulator)

handleEmulator :: 
    Simulator Emulator
    ~> Eff '[State (SchedulerState effs)]
handleEmulator = \case
    RunLocal wllt localAction -> undefined
    RunGlobal globalAction -> undefined

-- | Run the threads round-robin style, advancing the clock whenever there is
--   nothing left to do in the current slot.
runScheduler :: forall effs. SchedulerState effs -> Eff effs ()
runScheduler s@SchedulerState{currentSlotActions, futureActions, currentSlot} = do
    case currentSlotActions of
        t@EmThread{emContinuation}:xs -> do
            x' <- emContinuation currentSlot
            let newState = case x' of
                    Done () -> s{currentSlotActions = xs, futureActions = futureActions}
                    Continue ThisSlot r -> s{currentSlotActions = xs ++ [t{emContinuation=r}], futureActions = futureActions}
                    Continue (NextSlot _) r -> s{currentSlotActions = xs, futureActions = futureActions ++ [t{emContinuation=r}]}
            runScheduler newState
        [] -> do
             -- current slot is finished
             let newSlot = currentSlot + 1
             -- TODO: tell the node to do its thing and make a block
             runScheduler $ SchedulerState{currentSlotActions=futureActions, futureActions=[], currentSlot=newSlot}

