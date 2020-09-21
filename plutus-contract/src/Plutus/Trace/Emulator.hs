{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.Trace.Emulator(
    Emulator
    , ContractHandle(..)
    , activateContract
    , callEndpoint
    , payToWallet
    , waitUntilSlot
    , etrace
    ) where

import           Control.Monad.Freer
import           Data.Proxy               (Proxy (..))
import           Data.Void                (Void)
import           Language.Plutus.Contract (type (.\/), BlockchainActions, Contract, Endpoint, HasEndpoint)
import           Ledger.Slot              (Slot)
import           Ledger.Value             (Value)
import           Wallet.Emulator.Wallet   (Wallet (..))
import           Wallet.Types             (ContractInstanceId)

import           Plutus.Trace.Types

data Emulator

-- | A reference to an installed contract in the emulator.
data ContractHandle s e =
    ContractHandle
        { chContract   :: Contract s e ()
        , chInstanceId :: ContractInstanceId
        }

data EmulatorLocal r where
    ActivateContract :: Contract s e () -> EmulatorLocal (ContractHandle s e)
    CallEndpointEm :: forall l ep s e. HasEndpoint l ep s => Proxy l -> ContractHandle s e -> ep -> EmulatorLocal ()
    PayToWallet :: Wallet -> Value -> EmulatorLocal ()

data EmulatorGlobal r where
    WaitUntilSlot :: Slot -> EmulatorGlobal ()

instance SimulatorBackend Emulator where
    type LocalAction Emulator = EmulatorLocal
    type GlobalAction Emulator = EmulatorGlobal
    type Agent Emulator = Wallet

type EmulatorTrace a = Eff '[Simulator Emulator] a

activateContract :: forall s e. Wallet -> Contract s e () -> EmulatorTrace (ContractHandle s e)
activateContract wallet = send @(Simulator Emulator) . RunLocal wallet . ActivateContract

callEndpoint :: forall l ep s e. HasEndpoint l ep s => Wallet -> ContractHandle s e -> ep -> EmulatorTrace ()
callEndpoint wallet hdl = send @(Simulator Emulator) . RunLocal wallet . CallEndpointEm (Proxy @l) hdl

payToWallet :: Wallet -> Wallet -> Value -> EmulatorTrace ()
payToWallet from_ to_ =
    send @(Simulator Emulator) . RunLocal from_ . PayToWallet to_

waitUntilSlot :: Slot -> EmulatorTrace ()
waitUntilSlot sl = send @(Simulator Emulator) $ RunGlobal (WaitUntilSlot sl)

myContract :: Contract (BlockchainActions .\/ Endpoint "my endpoint" Int) Void ()
myContract = undefined

etrace :: EmulatorTrace ()
etrace = do
    runningCon <- activateContract (Wallet 1) myContract
    callEndpoint @"my endpoint" (Wallet 1) runningCon 10
