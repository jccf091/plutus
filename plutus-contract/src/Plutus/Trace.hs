{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Plutus.Trace(
    SimulatorBackend(..)
    , GlobalAction(..)
    , LocalAction(..)
    --  * Playground
    , Playground
    ) where

import Ledger.Slot (Slot)
import Control.Monad.Freer (Eff)
import qualified Data.Aeson as JSON
import Wallet.Emulator.Wallet (Wallet(..))
import Wallet.Types (ContractInstanceId)
import Data.Void (Void)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity)

class SimulatorBackend a where
  type InstanceId a
  type ContractId a
  type Agent a
  type Contract a
  type Assertion a
  type EndpointId a :: * -> *
  type EndpointValue a :: * -> *

-- | Actions that affect the global state. These are only available in
--   simulated environments.
data GlobalAction a r where
    WaitForSlot
        :: Slot
        -> GlobalAction a ()
    LocalAction
        :: Agent a
        -> LocalAction a r
        -> GlobalAction a r
    CheckAssertion
        :: Assertion a -- ?
        -> GlobalAction a ()

-- | Actions that affect the local state of a simulated agent.
data LocalAction a r where
    CallEndpoint
        :: InstanceId a
        -> EndpointId a (InstanceId a)
        -> EndpointValue a (EndpointId a (InstanceId a))
        -> LocalAction a ()
    Activate
        :: ContractId a
        -> LocalAction a (InstanceId a)
    Install
        :: Contract a
        -> LocalAction a (ContractId a)

data Playground -- TODO: Type var for schema? Since we only have a single contract with a known schema in the playground

instance SimulatorBackend Playground where
    type InstanceId Playground = () -- There is only one instance
    type Agent Playground = Wallet
    type EndpointId Playground = Const String
    type EndpointValue Playground = Const JSON.Value
    type ContractId Playground = ()

    type Assertion Playground = Void
    type Contract Playground = Void

-- | Playground traces need to be serialisable, so they are just
--   lists of single 'GlobalAction's.
type PlaygroundAction = GlobalAction Playground ()
type PlaygroundTrace = [PlaygroundAction]

ptrace :: PlaygroundTrace
ptrace = 
    [ LocalAction (Wallet 1) (CallEndpoint () (Const "submit") (Const $ JSON.toJSON "100 Ada"))
    , WaitForSlot 10
    ]

data Emulator

data SampleContracts =  Game | Crowdfunding

data SampleContract a (b :: SampleContracts) where
    AGame :: a -> SampleContract a Game
    ACrowdfunder :: a -> SampleContract a Crowdfunding

data SomeSampleContract a where
    SomeSampleContract :: forall a b. SampleContract a b -> SomeSampleContract a

instance SimulatorBackend Emulator where
    type Contract Emulator = SampleContracts
    type InstanceId Emulator = SomeSampleContract ContractInstanceId
    type Agent Emulator = Wallet
    type Contract Emulator = SampleContracts
    type Assertion Emulator = Void -- FIXME
    type EndpointId Emulator = Identity

type EmulatorTrace a = Eff '[GlobalAction Emulator] a

