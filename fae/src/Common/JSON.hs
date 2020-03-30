{- |
Module: Common.JSON
Description: JSON instances for encoding transaction summaries
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module exports (orphan) JSON instances and utilities for types that 
are generally useful both to the server (@faeServer@) and the client (@postTX@).
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Common.JSON where 

import Blockchain.Fae.FrontEnd
import Control.Applicative
import Control.DeepSeq
import Control.Exception (evaluate, throw)
import Control.Monad.Catch (catchAll)

import Data.Aeson (FromJSON, ToJSON, Object, toJSON,
   parseJSON, object, Value(..), withText,
   withObject, (.=), (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)

import System.IO.Unsafe

import Text.Read

-- | -
instance ToJSON TXInputSummary where
  toJSON TXInputSummary{..} = 
    object [
      "txInputStatus" .= wrapExceptions txInputStatus,
      "txInputOutputs" .= wrapExceptions txInputOutputs,
      "txInputMaterialsSummaries" .= txInputMaterialsSummaries,
      "txInputVersion" .= wrapExceptions txInputVersion ]

-- | -
instance ToJSON TXSummary where
  toJSON TXSummary{..} = object [
    "transactionID" .= transactionID,
    "txResult" .= wrapExceptions txResult,
    "txOutputs" .= wrapExceptions txOutputs,
    "txInputSummaries" .= txInputSummaries,
    "txMaterialsSummaries" .= txMaterialsSummaries,
    "txSSigners" .= txSSigners ]

-- | -
instance FromJSON TXInputSummary where
  parseJSON = withObject "TXInputSummary" $ \o -> do
    TXInputSummary
      <$> readJSONField "txInputStatus" o
      <*> readJSONField "txInputOutputs" o
      <*> o .: "txInputMaterialsSummaries"
      <*> readJSONField "txInputVersion" o
      
-- | -
instance FromJSON TXSummary where
  parseJSON = withObject "TXSummary" $ \o ->
    TXSummary
      <$> o .: "transactionID"
      <*> readJSONField "txResult" o
      <*> readJSONField "txOutputs" o
      <*> o .: "txInputSummaries"
      <*> o .: "txMaterialsSummaries"
      <*> o .: "txSSigners"

-- | -
instance FromJSON PublicKey where
  parseJSON = readJSONText "PublicKey" 

-- | -
instance ToJSON PublicKey where
  toJSON = toJSON . T.pack . show

-- | -
instance ToJSON ContractID where
  toJSON = toJSON . show

-- | -
instance FromJSON ContractID where
  parseJSON = readJSONText "ContractID" 

-- | -
instance ToJSON Digest where
  toJSON = toJSON . show

-- | -
instance FromJSON Digest where
  parseJSON = readJSONText "Digest" 

-- | -
instance ToJSON UnquotedString where
  toJSON = toJSON . show

-- | -
instance FromJSON UnquotedString where
  parseJSON = fmap UnquotedString . parseJSON

-- | -
instance ToJSON Status
-- | -
instance FromJSON Status
    
-- | Chains a couple conversions to get a readable json string.
encodeJSON :: (ToJSON a) => a -> String
encodeJSON = T.unpack . T.decodeUtf8 . BS.toStrict . A.encode

-- | If an exception is found then we tag the value as an exception.
-- By forcing evaluation of exceptions we prevent uncaught exceptions being thrown
-- and crashing faeServer.
wrapExceptions :: forall a. (ToJSON a) => a -> Value
wrapExceptions val = 
  unsafePerformIO $ catchAll (evaluate $ force $ toJSON val)
    (return . object . pure . ("exception",) . A.String . T.pack . show)

-- | If parsing fails then we look for the tagged exception.
readJSONField :: forall a. (FromJSON a) => Text -> Object -> Parser a
readJSONField fieldName obj = 
  obj .: fieldName <|> (obj .: fieldName >>= exceptionValue) 

-- | Reads a value from a JSON text expression.
readJSONText :: (Read a) => String -> Value -> Parser a
readJSONText l = withText l $ \t -> return $
  let s = T.unpack t in
  fromMaybe (throw $ JSONException s) $ readMaybe s 

-- | Parses a tagged exception.
exceptionValue :: Object -> Parser a
exceptionValue x = throw . TXFieldException <$> x .: "exception"

