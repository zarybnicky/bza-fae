{-# LANGUAGE ScopedTypeVariables #-}

module MerkleTree
  ( DecodingError(..)
  , MerkleProof(..)
  , mkMerkleProof
  , validateMerkleProof
  ) where

import Data.ByteString (ByteString)
import Data.Serialize (decode, encode)
import Control.Monad.Catch (Exception(..), MonadCatch, throwM)
import qualified Crypto.Hash.MerkleTree as MT

type MerkleProof = (ByteString, ByteString, ByteString) -- encoded proof of inclusion, root hash, jth leaf hash

newtype DecodingError = DecodingError String deriving Show

instance Exception DecodingError

mkMerkleProof :: [ByteString] -> ByteString -> MerkleProof
mkMerkleProof leafs leaf = (proof, rootHash, leaf)
  where
    tree = MT.mkMerkleTree leafs
    proof = encode $ MT.merkleProof tree (MT.mkLeafRootHash leaf)
    rootHash = MT.mtHash tree

validateMerkleProof :: MonadCatch m => MerkleProof -> m Bool
validateMerkleProof (proof, root, leaf) =
  case decode proof of
    Left msg -> throwM $ DecodingError msg
    Right decoded ->
      pure $
      MT.validateMerkleProof
        decoded
        (MT.MerkleRoot root)
        (MT.mkLeafRootHash leaf)
