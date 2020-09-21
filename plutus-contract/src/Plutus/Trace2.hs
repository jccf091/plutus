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
module Plutus.Trace2 where

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

-- Simulator a
data ThreadEff effs r where
  StartThread :: Eff (ThreadEff effs ': effs) () -> ThreadEff effs ()
  PrintMsg :: String -> ThreadEff effs ()

newtype Thread effs = Thread (Eff (ThreadEff effs ': effs) ())

-- handleEmulator
handleThread :: forall effs.
  (LastMember IO effs)
  => ThreadEff effs 
  ~> Eff (Yield (Thread effs) () ': effs)
handleThread = \case
  PrintMsg m -> sendM $ putStrLn m
  StartThread t -> yield (Thread t) id

runScheduler :: forall effs.
  LastMember IO effs
  => Eff (Yield (Thread effs) () ': effs) ()
  -> Eff effs ()
runScheduler e = loop [runC e] where

  -- SchedulerState
  loop :: [Eff effs (Status effs (Thread effs) () ())] -> Eff effs ()
  loop readyQ = case readyQ of
    [] -> pure ()
    (x:xs) -> do
      result <- x
      case result of
        Done _ -> loop xs
        Continue (Thread t) k -> do
          let t' = runC $ interpret handleThread (raiseUnder @effs @(ThreadEff effs) t)
              k' = k ()
          loop (xs ++ [t', k'])
