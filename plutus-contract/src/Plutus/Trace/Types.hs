{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Trace.Types
  ( SimulatorBackend (..),
    Simulator (..),

    -- * Handling the 'Simulator' effect
    SimulatorInterpreter (..),
    SuspendedThread (..),
    SlotChangeHandler (..),
    EmThread (..),
    ContinueWhen (..),
    handleEmulator,
    runSimulator,

    -- * Creating threads
    suspendNow,
    suspendFuture,
  )
where

import           Control.Lens
import           Control.Lens.TH               (makeLensesFor)
import           Control.Monad                 (unless)
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Error     (Error, throwError)
import           Control.Monad.Freer.Extras
import           Control.Monad.Freer.State     (State, get, gets, modify, put)
import qualified Control.Monad.Freer.State     as State
import           Data.Foldable                 (traverse_)
import           Data.Hashable                 (Hashable)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Sequence                 (Seq)
import qualified Data.Sequence                 as Seq
import           GHC.Generics                  (Generic)
import           Ledger.Crypto                 (PubKey)
import           Ledger.Slot                   (Slot)

class SimulatorBackend a where
  type LocalAction a :: * -> *
  type GlobalAction a :: * -> *
  type Agent a

data Simulator a b where
  RunLocal :: SimulatorBackend a => Agent a -> LocalAction a b -> Simulator a b
  RunGlobal :: SimulatorBackend a => GlobalAction a b -> Simulator a b

data ContinueWhen
  = -- | Yield control, resuming computation in the same slot
    ThisSlot
  | -- | Sleep until the given slot
    NextSlot (Maybe Slot)
  deriving stock (Eq, Ord, Show)

newtype EmThread a effs = EmThread {emContinuation :: Eff effs (Status effs (SimulatorSystemCall a effs) () ())}

data SuspendedThread a effs
  = SuspendedThread
      { stWhen   :: ContinueWhen,
        stThread :: EmThread a effs
      }

-- | The "system calls" we can make when interpreting a 'Simulator' action.
data SimulatorSystemCall a effs
  = -- | Start a new thread, and continue with the current thread in the current slot.
    Fork (SuspendedThread a effs)
  | -- | Yield control to other threads in the current slot.
    YieldThisSlot
  | -- | Yield control to other threads, resuming only when a new slot has begun.
    YieldNextSlot

newtype SlotChangeHandler effs = SlotChangeHandler {runSlotChangeHandler :: Eff effs ()}

data SimulatorInterpreter a effs
  = SimulatorInterpreter
      { interpRunLocal     :: forall b. Agent a -> LocalAction a b -> Eff effs (b, SuspendedThread a effs),
        interpRunGlobal    :: forall b. GlobalAction a b -> Eff effs (b, SuspendedThread a effs),
        -- | Called when we are done with all actions in the current slot.
        interpOnSlotChange :: SlotChangeHandler effs
      }

handleEmulator ::
  forall a effs.
  SimulatorInterpreter a effs ->
  Simulator a
    ~> Eff (Yield (SimulatorSystemCall a effs) () ': effs)
handleEmulator SimulatorInterpreter {interpRunGlobal, interpRunLocal} = \case
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
    { stWhen = ThisSlot,
      stThread = EmThread action
    }

suspendFuture :: Eff effs (Status effs (SimulatorSystemCall a effs) () ()) -> SuspendedThread a effs
suspendFuture action =
  SuspendedThread
    { stWhen = NextSlot Nothing,
      stThread = EmThread action
    }

runSimulator ::
  forall a effs.
  SimulatorInterpreter a effs ->
  Eff '[Simulator a] () ->
  Eff effs ()
runSimulator i =
  runThreads (interpOnSlotChange i)
    . interpret (handleEmulator i)
    . raiseEnd

runThreads ::
  forall a effs.
  SlotChangeHandler effs ->
  Eff (Yield (SimulatorSystemCall a effs) () ': effs) () ->
  Eff effs ()
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
data SchedulerState a effs
  = SchedulerState
      { currentSlotActions :: Seq (EmThread a effs),
        futureActions      :: Seq (EmThread a effs)
      }

initialState :: SchedulerState a effs
initialState = SchedulerState Seq.empty Seq.empty

enqueue :: SuspendedThread a effs -> SchedulerState a effs -> SchedulerState a effs
enqueue SuspendedThread {stWhen, stThread} s =
  case stWhen of
    ThisSlot   -> s {currentSlotActions = currentSlotActions s Seq.|> stThread}
    NextSlot _ -> s {futureActions = futureActions s Seq.|> stThread}

-- | Result of calling 'dequeue'. Either a thread that is ready now or in the next slot,
--   or empty.
data SchedulerDQResult a effs
  = AThread ContinueWhen (EmThread a effs) (SchedulerState a effs)
  | NoMoreThreads

dequeue :: SchedulerState a effs -> SchedulerDQResult a effs
dequeue SchedulerState {currentSlotActions, futureActions} =
  case Seq.viewl currentSlotActions of
    x Seq.:< xs -> AThread ThisSlot x SchedulerState {currentSlotActions = xs, futureActions}
    Seq.EmptyL -> case Seq.viewl futureActions of
      x Seq.:< xs -> AThread (NextSlot Nothing) x SchedulerState {currentSlotActions = xs, futureActions = Seq.empty}
      Seq.EmptyL  -> NoMoreThreads

-- Version 2
-- Reified events

data ThreadId = Networking | UserInput | OtherThread Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data ThreadMsg = EndpointCall String String | OwnPkResponse PubKey

data InboxMsg = InboxMsg {imEnqueuedAt :: Slot, imMessage :: ThreadMsg}

data OutboxMsg = OutboxMsg {oRecipient :: ThreadId, oMsg :: ThreadMsg}

newtype Worker effs = Worker {unWorker :: InboxMsg -> Eff effs [OutboxMsg]}

data MBState effs
  = MBState
      { mbWorkers :: HashMap ThreadId (Worker effs),
        mbInboxes :: HashMap ThreadId (Seq InboxMsg)
      }

makeLensesFor [("mbWorkers", "workers"), ("mbInboxes", "inboxes")] ''MBState

data MBError =
    NoMessagesForThread ThreadId
    | NoWorkerForThread ThreadId

-- | Take the first message from the thread's inbox.
dequeueMsg :: forall effs. ThreadId -> Eff (State (MBState effs) ': Error MBError ': effs) InboxMsg
dequeueMsg tid = do
  (msg, rest) <- do
    r <- gets @(MBState effs) (view (inboxes . at tid)) >>= maybe (throwError $ NoMessagesForThread tid) pure
    case Seq.viewl r of
        Seq.EmptyL    -> throwError $ NoMessagesForThread tid
        a Seq.:< rest -> pure (a, rest)
  if (Seq.null rest)
    then modify @(MBState effs) (inboxes . at tid .~ Just rest)
    else modify @(MBState effs) (inboxes . at tid .~ Nothing)
  pure msg

-- | Route an 'OutboxMessage' to the inbox of its recipient
enqueueMsg :: forall effs. OutboxMsg -> Eff (State (MBState effs) ': Error MBError ': effs) ()
enqueueMsg OutboxMsg{oRecipient, oMsg} = modify @(MBState effs) (over (inboxes . at oRecipient) addToInbox) where
    inboxMessage = InboxMsg{imEnqueuedAt = 0, imMessage = oMsg}

    addToInbox :: Maybe (Seq InboxMsg) -> Maybe (Seq InboxMsg)
    addToInbox Nothing  = Just $ Seq.singleton inboxMessage
    addToInbox (Just s) = Just $ s Seq.|> inboxMessage

-- | Get the 'Worker effs' for a thread ID.
getThread :: forall effs. ThreadId -> Eff (State (MBState effs) ': Error MBError ': effs) (Worker effs)
getThread tid = do
    r <- gets @(MBState effs) (view $ workers . at tid)
    case r of
        Nothing -> throwError $ NoWorkerForThread tid
        Just w  -> pure w

-- | Take the first message for this worker, process it, and add the resulting
--   messages to the appropriate queues
handleWorker :: ThreadId -> Eff (State (MBState effs) ': Error MBError ': effs) ()
handleWorker tid = do
  msg <- dequeueMsg tid
  Worker thread <- getThread tid
  newMSGs <- raise $ raise $ thread msg
  traverse_ enqueueMsg newMSGs
  pure ()

-- | Process the first message in each inbox, until all inboxes are empty.
loop2 :: forall effs. Eff (State (MBState effs) ': Error MBError ': effs) ()
loop2 = do
  currentState <- get @(MBState effs)
  unless (HashMap.null $ mbInboxes currentState) $ do
    State.gets @(MBState effs) (HashMap.keysSet . view inboxes) >>= traverse_ handleWorker
    loop2
