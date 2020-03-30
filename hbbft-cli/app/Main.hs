{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (AssertionFailed(..), handle, try)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Vector as V
import Data.Maybe (catMaybes)
import Data.ByteString.Char8 (pack)
import ErasureCoding (decodeMessage, encodeByteString)
import MerkleTree
  ( DecodingError
  , MerkleProof
  , mkMerkleProof
  , validateMerkleProof
  )

main :: IO ()
main = readInput >>= encode >>= computeMerkleProofs >>= decode >>= verifyMerkleProofs

readInput :: IO (String, Int, Int)
readInput = do
  putStrLn "Enter a message:"
  message :: String <- getLine
  putStrLn "\nEnter a number of data shards (N - 2f):"
  dataShards :: Int <- readLn
  putStrLn "\nEnter a number of total shards (N):"
  totalShards :: Int <- readLn
  return (message, dataShards, totalShards)

encode :: (String, Int, Int) -> IO (Maybe ([ByteString], Int, Int, Int))
encode (message, dataShards, totalShards) = do
  encodedMessage <- encodeByteString dataShards totalShards (pack message)
  putStrLn $ "\nEncoded: " ++ show encodedMessage ++ "\n"
  return $ Just (toStrict . Binary.encode <$> encodedMessage, length message, dataShards, totalShards)

computeMerkleProofs ::
     Maybe ([ByteString], Int, Int, Int)
  -> IO (Maybe ([ByteString], Int, Int, Int, [MerkleProof]))
computeMerkleProofs Nothing = return Nothing
computeMerkleProofs (Just (encoded, size, dataShards, totalShards)) = do
  let proofs = map (mkMerkleProof encoded) encoded
  showRoot proofs
  return $ Just (encoded, size, dataShards, totalShards, proofs)

showRoot [] = return ()
showRoot ((_, root, _):_) =
  putStrLn $ "Merkle tree root hash: " ++ show root ++ "\n"

decode ::
     Maybe ([ByteString], Int, Int, Int, [MerkleProof])
  -> IO (Maybe ([ByteString], [MerkleProof]))
decode Nothing = return Nothing
decode (Just (encoded, size, dataShards, totalShards, proofs)) = do
  let partialShards = take (dataShards + 1) encoded
  putStrLn $ "Decoding from: " ++ show partialShards ++ "\n"
  putStrLn $ "Decoded shards: " ++ show partialShards
  putStr "Decoded the message back: "
  maybeDecoded <- try $ decodeMessage (Binary.decode . fromStrict <$> partialShards)
  case maybeDecoded of
    Left (AssertionFailed _) -> return Nothing
    Right message -> do
      putStrLn $ show message ++ "\n"
      return $ Just ([message], proofs)

verifyMerkleProofs :: Maybe ([ByteString], [MerkleProof]) -> IO ()
verifyMerkleProofs Nothing = return ()
verifyMerkleProofs (Just (leafs, proofs)) = mapM_ verifyMerkleProofs' (zip leafs proofs)

verifyMerkleProofs' (leaf, proof) = do
  putStrLn $ "Verifying proof " ++ show proof ++ " for the leaf " ++ show leaf ++ "."
  proof <- handle onProofDecodeFailure (validateMerkleProof proof)
  putStrLn $ "Successfully decoded the proof: " ++ show proof ++ "\n"

onProofDecodeFailure :: DecodingError -> IO Bool
onProofDecodeFailure e = do
  putStrLn $ "Failed to decode the proof: " ++ show e ++ "\n"
  return False
