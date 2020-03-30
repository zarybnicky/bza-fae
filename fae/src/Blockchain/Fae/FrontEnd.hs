{-# LANGUAGE Unsafe #-}
{- |
Module: Blockchain.Fae.FrontEnd
Description: The API for implementors of a Fae front-end
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

If you are writing a Fae client, this module is for you.  It exposes the
functions for running blocks and transactions, and for showing the storage.
Most of the re-exported modules were imported with restricted lists, but
due to a deficiency in Haddock this is not reflected in the generated
document.
-}
module Blockchain.Fae.FrontEnd
  (  -- * Messages
    module Blockchain.Fae.Internal.Messages
    -- * Interpreting transactions
  , module Blockchain.Fae.Internal.TX
    -- * Running transactions without interpreting
  , module Blockchain.Fae.Internal.Transaction
    -- * Fae storage types and storage access
  , module Blockchain.Fae.Internal.Storage
  , exportValue
  , importValue
    -- * Transaction evaluation
  , module Blockchain.Fae.Internal.TXSummary
  , module Blockchain.Fae.Internal.Monitors
    -- * Cryptography types and functions
  , module Blockchain.Fae.Internal.Crypto
    -- * Fae exceptions
  , module Blockchain.Fae.Internal.Exceptions
    -- * Fae ID types
  , module Blockchain.Fae.Internal.IDs.Types
  ) where

import Blockchain.Fae.Internal.Contract (exportValue, importValue)
import Blockchain.Fae.Internal.Crypto hiding
  ( PartialSerialize
  , PassFail
  , compareSerialize
  , getPartialSerialize
  , putPartialSerialize
  , readsPrecSer
  )
import Blockchain.Fae.Internal.Exceptions hiding (unsafeIsDefined)
import Blockchain.Fae.Internal.GenericInstances ()
import Blockchain.Fae.Internal.IDs.Types
import Blockchain.Fae.Internal.Messages hiding
  ( unsignTXMessage
  , unsignedTXMessage
  )
import Blockchain.Fae.Internal.TXSummary
  ( InputSummary
  , MaterialsSummaries
  , TXInputSummary(..)
  , TXSummary(..)
  , collectTransaction
  )
import Blockchain.Fae.Internal.Monitors
import Blockchain.Fae.Internal.Storage hiding
  ( onlyJust
  , txInputLens
  , txPartLens
  , uncertain
  , vectorAt
  )
import Blockchain.Fae.Internal.Transaction
  ( Input(..)
  , InputMaterials
  , TXStorageM
  , TransactionBody(..)
  , runTransaction
  )
import Blockchain.Fae.Internal.TX

