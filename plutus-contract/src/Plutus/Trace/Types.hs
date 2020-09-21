{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Trace.Types(
    SimulatorBackend(..)
    , Simulator(..)
    -- * Handling the 'Simulator' effect
    , SimulatorInterpreter(..)
    , SuspendedThread(..)
    , SlotChangeHandler(..)
    , EmThread(..)
    , ContinueWhen(..)
    , handleEmulator
    , runSimulator
    -- * Creating threads
    , suspendNow
    , suspendFuture
    ) where

import           Control.Monad                 (unless)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Extras
import           Data.Sequence                 (Seq)
import qualified Data.Sequence                 as Seq
import           Ledger.Slot                   (Slot)

class SimulatorBackend a where
  type LocalAction a :: * -> *
  type GlobalAction a :: * -> *
  type Agent a

data Simulator a b where
    RunLocal :: SimulatorBackend a => Agent a -> LocalAction a b -> Simulator a b
    RunGlobal :: SimulatorBackend a => GlobalAction a b -> Simulator a b

data ContinueWhen
    = ThisSlot -- ^ Yield control, resuming computation in the same slot
    | NextSlot (Maybe Slot) -- ^ Sleep until the given slot
    deriving stock (Eq, Ord, Show)

newtype EmThread a effs = EmThread { emContinuation ::  Eff effs (Status effs (SimulatorSystemCall a effs) () ()) }

data SuspendedThread a effs =
    SuspendedThread
        { stWhen   :: ContinueWhen
        , stThread :: EmThread a effs
        }

-- | The "system calls" we can make when interpreting a 'Simulator' action.
data SimulatorSystemCall a effs =
    Fork (SuspendedThread a effs)  -- ^ Start a new thread, and continue with the current thread in the current slot.
    | YieldThisSlot -- ^ Yield control to other threads in the current slot.
    | YieldNextSlot -- ^ Yield control to other threads, resuming only when a new slot has begun.

newtype SlotChangeHandler effs = SlotChangeHandler { runSlotChangeHandler :: Eff effs () }

data SimulatorInterpreter a effs =
    SimulatorInterpreter
        { interpRunLocal     :: forall b. Agent a -> LocalAction a b -> Eff effs (b, SuspendedThread a effs)
        , interpRunGlobal    :: forall b. GlobalAction a b -> Eff effs (b, SuspendedThread a effs)
        , interpOnSlotChange :: SlotChangeHandler effs -- ^ Called when we are done with all actions in the current slot.
        }

handleEmulator :: forall a effs.
    SimulatorInterpreter a effs
    -> Simulator a
    ~> Eff (Yield (SimulatorSystemCall a effs) () ': effs)
handleEmulator SimulatorInterpreter{interpRunGlobal, interpRunLocal} = \case
    RunLocal wllt localAction -> do
        (b, thread) <- raise $ interpRunLocal wllt localAction
        _ <- yield @(SimulatorSystemCall a effs) @() (Fork thread) id
        pure b
    RunGlobal globalAction -> do
        (b, thread) <- raise $ interpRunGlobal globalAction
        _ <- yield @(SimulatorSystemCall a effs) @() (Fork thread) id
        pure b

suspendNow :: Eff effs (Status effs (SimulatorSystemCall a effs) () ()) -> SuspendedThread a effs
suspendNow action =
    SuspendedThread
        { stWhen = ThisSlot
        , stThread = EmThread action
        }

suspendFuture :: Eff effs (Status effs (SimulatorSystemCall a effs) () ()) -> SuspendedThread a effs
suspendFuture action =
    SuspendedThread
        { stWhen = NextSlot Nothing
        , stThread = EmThread action
        }

runSimulator :: forall a effs.
    SimulatorInterpreter a effs
    -> Eff '[Simulator a] ()
    -> Eff effs ()
runSimulator i =
    runThreads (interpOnSlotChange i)
    . interpret (handleEmulator i)
    . raiseEnd

runThreads :: forall a effs.
    SlotChangeHandler effs
    -> Eff (Yield (SimulatorSystemCall a effs) () ': effs) ()
    -> Eff effs ()
runThreads handler e =
    loop handler $ enqueue (suspendNow $ runC e) initialState

-- | Run the threads that are scheduled in a 'SchedulerState' to completion,
--   calling the 'SlotChangeHandler' whenever there is nothing left to do in
--   the current slot.
loop :: SlotChangeHandler effs -> SchedulerState a effs -> Eff effs ()
loop handler s = do
    case dequeue s of
        AThread w (EmThread x) newState -> do
            unless (w == ThisSlot) (runSlotChangeHandler handler)
            result <- x
            case result of
                Done _ -> loop handler newState
                Continue thread k -> do
                    let newState' = case thread of
                            Fork thread'  -> enqueue thread' $ enqueue (suspendNow $ k ()) $ newState
                            YieldThisSlot -> enqueue (suspendNow $ k ()) newState
                            YieldNextSlot -> enqueue (suspendFuture $ k ()) newState
                    loop handler newState'
        NoMoreThreads -> pure ()

-- | Scheduler state consisting of two queues of suspended threads: One with
--   threads that can be resumed in the current slot, and the other with
--   threads that can only be resumed after the current slot.
data SchedulerState a effs =
    SchedulerState
        { currentSlotActions :: Seq (EmThread a effs)
        , futureActions      :: Seq (EmThread a effs)
        }

initialState :: SchedulerState a effs
initialState = SchedulerState Seq.empty Seq.empty

enqueue :: SuspendedThread a effs -> SchedulerState a effs -> SchedulerState a effs
enqueue SuspendedThread{stWhen, stThread} s =
    case stWhen of
        ThisSlot   -> s{currentSlotActions = currentSlotActions s Seq.|> stThread }
        NextSlot _ -> s{futureActions=futureActions s Seq.|> stThread}

-- | Result of calling 'dequeue'. Either a thread that is ready now or in the next slot,
--   or empty.
data SchedulerDQResult a effs =
    AThread ContinueWhen (EmThread a effs) (SchedulerState a effs)
    | NoMoreThreads

dequeue :: SchedulerState a effs -> SchedulerDQResult a effs
dequeue SchedulerState{currentSlotActions, futureActions} =
    case Seq.viewl currentSlotActions of
        x Seq.:< xs -> AThread ThisSlot x SchedulerState{currentSlotActions=xs, futureActions}
        Seq.EmptyL -> case Seq.viewl futureActions of
            x Seq.:< xs -> AThread (NextSlot Nothing) x SchedulerState{currentSlotActions=xs, futureActions=Seq.empty}
            Seq.EmptyL -> NoMoreThreads
