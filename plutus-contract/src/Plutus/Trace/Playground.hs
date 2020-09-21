{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Plutus.Trace.Playground(
    Playground
    , PlaygroundLocal(..)
    , PlaygroundGlobal(..)
    , ptrace
    ) where

import qualified Data.Aeson             as JSON
import           Ledger.Slot            (Slot)
import           Ledger.Value           (Value)
import           Wallet.Emulator.Wallet (Wallet (..))

import           Plutus.Trace.Types

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

ptrace :: PlaygroundTrace
ptrace =
    [ RunLocal (Wallet 1) $ CallEndpoint "submit" (JSON.toJSON "100 Ada")
    , RunGlobal $ WaitForSlot 10
    ]
