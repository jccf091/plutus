{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Symbolic.Types.Response where

import           Control.Applicative        ((<*>), (<|>))
import           Data.Aeson                 (object, parseJSON, toJSON, toJSONList, withArray, (.:), (.=))
import qualified Data.Aeson                 as JSON
import qualified Data.Foldable              as F
import           Data.Text                  (pack, unpack)
import           Deriving.Aeson
import           GHC.Generics
import           Language.Marlowe.Semantics (TransactionInput, TransactionWarning)

data Result = Valid
            | CounterExample
                { initialSlot        :: Integer
                , transactionList    :: [TransactionInput]
                , transactionWarning :: [TransactionWarning]
                }
            | Error String
  deriving (Generic)

instance FromJSON Result where
  parseJSON (JSON.Object v) =
        (CounterExample <$> (parseJSON =<< (v .: "initial_slot"))
                        <*> ((v .: "inputs") >>=
                             withArray "Input list" (\bl ->
                               mapM parseJSON (F.toList bl)
                                                    ))
                        <*> ((v .: "warnings") >>=
                             withArray "Input list" (\bl ->
                               mapM parseJSON (F.toList bl)
                                                    )))
    <|> (Error . unpack <$> (v .: "error"))
  parseJSON (JSON.String "valid") = return Valid
  parseJSON _ = fail "Result must be an object or the string \"valid\""

instance ToJSON Result where
  toJSON Valid = JSON.String $ pack "valid"
  toJSON (CounterExample iSlot tList tWarn) = object
      [ "initial_slot" .= iSlot
      , "inputs" .= toJSONList (map toJSON tList)
      , "warnings" .= toJSONList (map toJSON tWarn)
      ]
  toJSON (Error err) = object
      [ "role_token" .= (JSON.String $ pack err) ]

