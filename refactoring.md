
```haskell
-- Playground:

type SimulatorAction = ContractCall (Fix FormArgumentF)

type Expression = ContractCall JSON.Value

data Simulation =
    Simulation
        { simulationName    :: String
        , simulationActions :: [SimulatorAction]
        , simulationWallets :: [SimulatorWallet]
        }
    deriving (Show, Generic, Eq)
    deriving anyclass (ToJSON, FromJSON)
```

# Qs

- Playground agents are wallets, not contract instances?
- Assertions?
- Await / events
  * Peng Li, Steve Zdancewic: "A Language-based Approach to Unifying Events and Threads"
  * What are we waiting for
    - A given slot
    - An address to be modified
  * Threads = Contract instances, blockchain, UI
- Traces, extracting a visual representation of a simulation
  - using the Observations effect?
  ```plantuml
  @startuml
  Alice -> Bob : message 1
  Alice -> Bob : message 2

  newpage

  Alice -> Bob : message 3
  Alice -> Bob : message 4

  newpage A title for the\nlast page

  Alice -> Bob : message 5
  Alice -> Bob : message 6
  @enduml
  ```
- Clock ticks
- No more handleBlockchainEvents?
  * 3 queues: this slot, next slot, specific slot?
  * Skip contract notifications
- Global ops for simulator:
  - Advance clock (?) OR: Wait until slot
  - Suspend contract updates
  - Resume contract updates
  - User input / call endpoint
- Scheduler
  - In simulator: Advance clock to given slot
  - In real-time mode / scb: Wait until the slot has been reached
- Calling endpoint = Wait for endpoint to become available (just like "Await slot")
  - Would be nice to get a state transition diagram about which endpoints can be called in which order
  - Selective functor
- Hedgehog generator for simulations

# Approach

```haskell

class SimulatorEnvironment a where
  type ContractInstanceIdentifier a
  type ContractIdentifier a
  type Agent a
  type Contract a
  type Assertion a
  type ContractEndpointIdentifier a :: Contract a -> *

-- | An action that runs in a global context.
data GlobalAction a r where
  WaitForSlot :: Slot -> GlobalAction a ()
  LocalAction :: Agent a -> LocalAction a r -> GlobalAction a r
  CheckAssertion :: Assertion a -> GlobalAction a ()

-- | An action that runs in the context of one of the simulated agents.
data LocalAction a r where
  CallEndpoint :: ContractInstanceIdentifier a -> String -> JSON.Value -> LocalAction a ()
  Activate :: ContractIdentifier a -> GlobalAction a (ContractInstanceIdentifier a)
  Install :: Contract a -> GlobalAction a (ContractIdentifier a)

data Playground

instance SimulatorEnvironment Playground where
  type ContractInstanceIdentifier Playground = ()
  type Agent Playground = Wallet
  type ContractEndpointIdentifier Playground = forall a. String

  type ContractIdentifier Playground = Void -- We can't activate contracts in the playground
  type Contract Playground = Void -- We can't install contracts in the playground
  type Assertion Playground = Void -- We can't make assertions in the playground

instance ToJSON (GlobalAction Playground) where
  ...

instance FromJSON (GlobalAction Playground) where

data Emulator

instance 

```

