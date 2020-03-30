{- |
Module: Blockchain.Fae.Internal.TXSummary
Description: Displaying Fae storage
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module defines a 'TXSummary' type as well as 'Pretty' instances.
-}
{-# LANGUAGE RecordWildCards #-}

module Blockchain.Fae.Internal.TXSummary where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs.Types
import Blockchain.Fae.Internal.Monitors
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.TX
import Common.Lens
import System.IO.Unsafe
import Control.DeepSeq
import Control.Exception (SomeException, evaluate, throw)
import Control.Monad
import Control.Monad.Catch (MonadCatch, catchAll)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Foldable
import GHC.Generics hiding (to)
import Prelude hiding ((<>))
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

-- | Heavy-handed total information about the result of a transaction, in
-- a relatively printable format
data TXSummary = TXSummary {
  transactionID :: TransactionID,
  txResult :: String,
  txOutputs:: Vector VersionID,
  txInputSummaries :: Vector InputSummary,
  txMaterialsSummaries :: MaterialsSummaries,
  txSSigners :: [(String, PublicKey)]
} deriving (Generic)

-- | Printable results of calling a contract.
data TXInputSummary = TXInputSummary {
  txInputStatus :: Status,
  txInputOutputs :: Vector VersionID,
  txInputMaterialsSummaries :: MaterialsSummaries,
  txInputVersion :: VersionID
} deriving (Generic)

-- | The 'InputResults' contained the contract ID but the 'TXInputSummary'
-- does not, because we want to print the ID regardless of whether the
-- input summary throws an error, so we include it separately.
type InputSummary = (ContractID, TXInputSummary)
-- | Materials, in addition, are given names under the contract that calls
-- them.
type MaterialsSummaries = Vector (String, InputSummary)

{- Instances -}

-- | -
instance Pretty ContractID where 
  pPrint = text . prettyContractID

-- | -
instance Pretty TXSummary where
  pPrint TXSummary{..} = prettyHeader header entry where 
    header = labelHeader "Transaction" transactionID
    result = prettyPair ("result", displayException $ text txResult)
    outputs = prettyVector "outputs" txOutputs
    txSSigners' = prettyList "signers" txSSigners
    inputs = 
      vcat . toList . Vector.imap (printInputSummary . makeTag) $ txInputSummaries 
      where makeTag = ("input #" ++) . show
    materials = printMaterialsSummaries txMaterialsSummaries
    entry = vcat [outcome, rest]
    outcome = displayException $ vcat [result, outputs] 
    rest = vcat [txSSigners', inputs, materials]

-- | -
instance Pretty TXInputSummary where
  pPrint TXInputSummary{..} = vcat
    [
      displayException (vcat $ pVersion [outputs]), 
      materials
    ]
    where
      pVersion
        | Updated <- txInputStatus = (prettyPair ("version", txInputVersion) :)
        | otherwise = id
      outputs = prettyVector "outputs" txInputOutputs
      materials = printMaterialsSummaries txInputMaterialsSummaries

-- | This occurs twice, so it's worth having its own function.
printMaterialsSummaries :: MaterialsSummaries -> Doc
printMaterialsSummaries = 
  vcat . toList . fmap (uncurry $ printInputSummary . makeTag) 
  where makeTag name = "material '" ++ name ++ "'" 

-- | This occurs twice, so it's worth having its own function.
printInputSummary :: String -> InputSummary -> Doc
printInputSummary tag (cID, txInputSummary@TXInputSummary{..}) = 
  prettyHeader (prettyPair (tag, printCID)) prettyInput
  where
    printCID = pPrint cID <+> parens (text $ show txInputStatus) 
    prettyInput = pPrint txInputSummary

-- * Functions

-- | Get a well-typed 'TXSummary' that can be communicated from the server
-- to a user (i.e. @faeServer@ to @postTX@) as JSON.
collectTransaction :: 
  (MonadState Storage m, MonadCatch m, MonadIO m) => 
  TransactionID -> EvalT m TXSummary
collectTransaction txID = do
  TransactionEntry{..} <- 
    use $ _getStorage . at txID . defaultLens (throw $ BadTransactionID txID)
  let transactionID = txID
      txSSigners = Map.toList $ getSigners txSigners
  txMaterialsSummaries <- makeMaterialsSummaries txID inputMaterials
  txInputSummaries <- Vector.imapM (makeInputSummary txID . makeTag) inputResults
  ~(txResult, txOutputs) <- evalArg (show result, makeOut <$> outputs)
  return TXSummary{..}

  where makeTag = ("Input contract call #" ++) . show

-- | Get the 'TXInputSummary' for a given 'TransactionID' 
makeInputSummary :: 
  (Monad m) => TransactionID -> String -> InputResults -> EvalT m InputSummary
makeInputSummary txID descr InputResults{..} = do
  txInputMaterialsSummaries <- 
    maybe (return err) (makeMaterialsSummaries txID) iMaterialsM
  ~(txInputVersion, outputsM, status) <- evalArg (iVersionID, iOutputsM, iStatus)
  let txInputOutputs = maybe err (makeOut <$>) outputsM
      txInputStatus | unsafeIsDefined status = status
                    | otherwise = Failed
  return (iRealID, TXInputSummary{..})

  where 
    err :: a
    err = throw $ ContractOmitted txID descr

-- | Converts materials as 'InputResults' to appropriately error-formatted
-- summaries.
makeMaterialsSummaries :: 
  (Monad m) => TransactionID -> Materials -> EvalT m MaterialsSummaries
makeMaterialsSummaries txID = traverse $ 
  \(name, iR) -> (name,) <$> makeInputSummary txID (makeTag name) iR
  where makeTag = ("Materials contract call " ++)

-- | If outputs showed more detailed information, like contract types, this
-- would construct it.  As it is, it just grabs the version.
makeOut :: Output -> VersionID
-- We assume (justifiably!) that a new output does, in fact, store a live
-- contract.
makeOut Output{storedContract=Just StoredContract{..},..} = storedVersion

-- | Constructs a header with a name and some other data.
labelHeader :: (Show a) => String -> a -> Doc
labelHeader h l = text h <+> text (show l)

-- | Formats a nice header with indented body.
prettyHeader :: Doc -> Doc -> Doc
prettyHeader header body = header $+$ nest 2 body 

-- | Prints a vector as an @Int@-indexed list.
prettyVector :: (Show v) => String -> Vector v -> Doc
prettyVector headString v 
  | Vector.null v = empty
  | otherwise = prettyList headString $ Vector.toList $ Vector.imap ((,) . show) v

-- | Converts a string and list of "lines" into a body with header.
prettyList :: (Show v) => String -> [(String, v)] -> Doc
prettyList headString bodyList = 
  prettyHeader (text headString <> colon) (displayException $ prettyPairs bodyList)

-- | Converts a list of pairs into a display, without header.
prettyPairs :: (Show v) => [(String, v)] -> Doc
prettyPairs = vcat . map prettyPair

-- | Prints a key-value pair with a colon.
prettyPair :: (Show v) => (String, v) -> Doc
prettyPair (x, y) = text x <> colon <+> text (show y)

-- | Actually prints the exception nicely.  Due to call stack cruft we only
-- take the first line.
showException :: SomeException -> Doc
showException e = text "<exception>" <+> text (safeHead $ lines $ show $ UnquotedString $ show e) where
  safeHead [] = []
  safeHead (x : _) = x

-- | Flushes out all exceptions present in the input, returning a formatted
-- error message if one is found.
displayException :: Doc -> Doc
displayException doc = 
  unsafePerformIO $ catchAll (evaluate $ force doc) (return . showException) 

