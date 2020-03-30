{- |
Module: Blockchain.Fae.Internal.Exceptions
Description: Wrapper library for "Control.Monad.Catch"
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module just re-exports "Control.Monad.Catch", as well as 'Typeable' so that we can derive 'Exception' with just this module imported, and also 'throw' and 'evaluate' from "Control.Exception", which seem not to be re-exported elsewhere.
-}
module Blockchain.Fae.Internal.Exceptions where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.IDs.Types
import Control.Exception (evaluate)
import Control.Monad.Catch (Exception, catchAll)
import Data.ByteString (ByteString)
import Data.Typeable as T
import System.IO.Unsafe

-- * Functions

-- | Obviously not pure at all, but intended to be used only in extremely
-- limited circumstances; namely, to decide what to do with the global
-- update of a contract function or nonce, or escrow function, in the event
-- of an exception.
unsafeIsDefined :: a -> Bool
unsafeIsDefined act = unsafePerformIO $ catchAll 
  (evaluate act >> return True) 
  (const $ return False)

-- * Types

-- | Exceptions for storage-related errors.
data StorageException =
  BadTransactionID TransactionID |
  BadContractID ContractID |
  BadInputID TransactionID Int |
  BadVersion ContractID Int Int |
  InvalidVersionAt ContractID |
  ContractOmitted TransactionID String |
  CantImport ByteString TypeRep |
  ImportWithoutVersion ContractID |
  NotExportable TypeRep |
  DeletedEntry 

-- | Exceptions for contract-related errors.
data ContractException =
  ContractDeleted ContractID |
  BadContractVersion VersionID ContractID |
  BadInputParse String TypeRep |
  BadArgType TypeRep TypeRep | 
  BadValType TypeRep TypeRep |
  BadMaterialType String TypeRep TypeRep |
  MissingMaterial String |
  BadEscrowID EntryID |
  BadEscrowName EntryID TypeRep TypeRep |
  MissingSigner String |
  NotStartState EntryID VersionID

-- | Exceptions for transaction-related errors.
data TransactionException =
  NotEnoughInputs |
  UnexpectedInput |
  ExpectedReward |
  UnexpectedReward |
  BadSignature |
  InputFailed ContractID |
  EmptyInputStack |
  RepeatedMaterial String

-- | Exceptions arising non-core UI components.
data DisplayException = 
  InterpretException String |
  TXFieldException String |
  JSONException String |
  MonitorException String |
  Timeout Int

-- * Instances

-- | -
instance Show StorageException where
  show (BadTransactionID tID) = "Not a transaction ID: " ++ show tID
  show (BadContractID cID) = "Not a contract ID: " ++ prettyContractID cID
  show (BadInputID txID ix) = 
    "No input contract with index " ++ show ix ++ 
    " for transaction " ++ show txID
  show (BadVersion cID bad good) = 
    "Contract " ++ prettyContractID cID ++ 
    " has nonce " ++ show good ++ "; got: " ++ show bad
  show (InvalidVersionAt cID) = "Can't look up contract ID: " ++ prettyContractID cID
  show (ContractOmitted txID descr) =
    descr ++ " in transaction " ++ show txID ++ 
    " was replaced with an imported return value."
  show (CantImport bs ty) =
    "Can't decode value of type " ++ show ty ++ " from bytes: " ++ printHex bs
  show (ImportWithoutVersion cID) =
    "Rejecting imported value for " ++ prettyContractID cID ++ 
    " that lacks a nonce value."
  show (NotExportable ty) = 
    "Type " ++ show ty ++ " cannot be imported or exported."
  show (DeletedEntry) =
    "(internal error) Tried to delete an entry of the transaction results!"

-- | -
instance Show ContractException where
  show (ContractDeleted cID) = 
    "Contract " ++ prettyContractID cID ++ " was deleted"
  show (BadContractVersion ver cID) =
    "Incorrect version in contract ID: " ++ prettyContractID cID ++
    "; correct version is: " ++ show ver
  show (BadInputParse input inputType) = 
    "Unable to parse '" ++ input ++ "' as type: " ++ show inputType
  show (BadArgType bad good) = 
    "Expected argument type: " ++ show good ++ "; got: " ++ show bad
  show (BadValType bad good) =
    "Expected value type: " ++ show good ++ "; got: " ++ show bad
  show (BadMaterialType name bad good) = 
    "Expected material '" ++ name ++ "' of type: " ++ show good ++ 
    "; got: " ++ show bad
  show (MissingMaterial name) = "No material named " ++ show name
  show (BadEscrowID eID) = "No escrow found in this contract with ID: " ++ show eID
  show (BadEscrowName entID bad good) =
    "Wrong contract name for escrow " ++ show entID ++ 
    "; got " ++ show bad ++ "; expected " ++ show good
  show (MissingSigner name) = "No signer named " ++ show name
  show (NotStartState entID vID) = 
    "Escrow " ++ show entID ++ 
    " with version " ++ show vID ++ 
    " is not in its starting state"

-- | -
instance Show TransactionException where
  show NotEnoughInputs = "Transaction expected more inputs"
  show UnexpectedInput = "Excess input given transaction body's signature"
  show ExpectedReward = "Transaction expected a reward as its first argument"
  show UnexpectedReward = "Transaction passed an unexpected reward"
  show BadSignature = "Transaction signature does not match contract return types"
  show (InputFailed cID) = 
    "Used the result of failed input contract " ++ prettyContractID cID 
  show EmptyInputStack = "(internal error) Tried to use an empty stack!"
  show (RepeatedMaterial name) = "Repeated material name '" ++ name ++ "'"

-- | -
instance Show DisplayException where
  show (InterpretException s) = s
  show (TXFieldException s) = s
  show (JSONException s) = "Error in JSON serialization: " ++ s
  show (MonitorException s) = "Error in monitor operation: " ++ s
  show (Timeout t) = "Exceeded timeout of " ++ show t ++ " milliseconds"

-- | -
instance Exception StorageException
-- | -
instance Exception ContractException
-- | -
instance Exception TransactionException
-- | -
instance Exception DisplayException
