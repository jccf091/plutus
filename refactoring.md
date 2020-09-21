# Contact trace refactoring

## Requirements

### What the trace type is used for

* plutus-use-cases: Run contracts in emulator, call endpoints in a type-safe fashion
* plutus-playground: Run 1 contract in the playground, serialisable traces
* plutus-scb: Run contracts in SCB

### What is annoying about the current implementation

* 2 full-blown emulators, one in plutus-scb and one in plutus-contract
* Having to call "handleBlockchainEvents" and "addBlock" - this is related to how time progresses in the emulator
* A lot of polling in handleBlockchainEvents and elsewhere

### What would be nice to have

* Run traces in interactive mode (using (mock) node, wallet, etc.)
* Extract UML sequence diagrams from trace (Plant UML)

## Implementation Plan

### Different kinds of traces

To address the fact that we have three somewhat-different trace versions use the following type, mixing "trees that grow" with freer effects:

```haskell
class SimulatorBackend a where
  type LocalAction a :: * -> *
  type GlobalAction a :: * -> *
  type Agent a

data Simulator a b where
    RunLocal :: SimulatorBackend a => Agent a -> LocalAction a b -> Simulator a b
    RunGlobal :: SimulatorBackend a => GlobalAction a b -> Simulator a b
```

Then we can instantiate this with different types depending on the trace. For example:

```haskell
data Playground

data PlaygroundLocal r where
   CallEndpoint :: String -> JSON.Value -> PlaygroundLocal ()
   PayToWallet :: Wallet -> Value -> PlaygroundLocal ()

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
```

and

```haskell
data Emulator
-- | A reference to an installed contract in the emulator.
data ContractHandle s e =
    ContractHandle
        { chContract   :: Contract s e ()
        , chInstanceId :: ContractInstanceId
        }

data EmulatorLocal r where
    ActivateContract :: Contract s e () -> EmulatorLocal (ContractHandle s e)

    -- calling endpoints with correctly typed arguments, using the schema
    CallEndpointEm :: forall l ep s e. HasEndpoint l ep s => Proxy l -> ContractHandle s e -> ep -> EmulatorLocal ()
    PayToWallet :: Wallet -> Value -> EmulatorLocal ()

data EmulatorGlobal r where
    WaitUntilSlot :: Slot -> EmulatorGlobal ()

instance SimulatorBackend Emulator where
    type LocalAction Emulator = EmulatorLocal
    type GlobalAction Emulator = EmulatorGlobal
    type Agent Emulator = Wallet

type EmulatorTrace a = Eff '[Simulator Emulator] a
```

### Dealing with time & polling

To address the issues re. time: Use a scheduling approach to avoid polling. Based on _Peng Li, Steve Zdancewic_: "A Language-based Approach to Unifying Events and Threads"

```haskell
-- | The "system calls" we can make when interpreting a 'Simulator' action.
data SimulatorSystemCall a effs =
    Fork (SuspendedThread a effs)  -- ^ Start a new thread, and continue with the current thread in the current slot.
    | YieldThisSlot -- ^ Yield control to other threads in the current slot.
    | YieldNextSlot -- ^ Yield control to other threads, resuming only when a new slot has begun.

newtype EmThread a effs = EmThread { emContinuation ::  Eff effs (Status effs (SimulatorSystemCall a effs) () ()) }
{- NB The 'Status' type is from 'Control.Monad.Freer.Coroutine' -}

data ContinueWhen
    = ThisSlot -- ^ Yield control, resuming computation in the same slot
    | NextSlot (Maybe Slot) -- ^ Sleep until the given slot
    deriving stock (Eq, Ord, Show)

data SuspendedThread a effs =
    SuspendedThread
        { stWhen   :: ContinueWhen
        , stThread :: EmThread a effs
        }

data SimulatorInterpreter a effs =
    SimulatorInterpreter
        { interpRunLocal     :: forall b. Agent a -> LocalAction a b -> Eff effs (b, SuspendedThread a effs)
        , interpRunGlobal    :: forall b. GlobalAction a b -> Eff effs (b, SuspendedThread a effs)
        , interpOnSlotChange :: SlotChangeHandler effs -- ^ Called when we are done with all actions in the current slot.
        }

newtype SlotChangeHandler effs = SlotChangeHandler { runSlotChangeHandler :: Eff effs () }
```

This handles the old "non-interactive" mode of running traces and also supports "interactive mode" as an extension (where we use the real system time in `interpOnSlotChange`). It also gets rid of `handleBlockchainEvents` and of the need to do polling since we can use the `ContinueWhen` type to indicate when a thread should be woken up again.
