{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module RBC
  ( In(..)
  , Out(..)
  , RBCError(..)
  , RBCState(..)
  , getErasureCodingScheme
  , markReadySent
  , receive
  , setup
  , storeEcho
  , storeMessage
  , storeReady
  ) where

-- Reliable Broadcast
--
-- For more details, see the article - https://eprint.iacr.org/2016/199.pdf
--
-- • upon input(v) (if Pi = PSender):
--   let{sj} j∈[N] be the blocks of an (N − 2f, N) erasure coding scheme applied to v
--   let h be a Merkle tree root computed over {sj}
--   send VAL(h,bj,sj) to each party Pj, where bj is the jth Merkle tree branch
-- • upon receiving VAL(h,bi,si) from PSender, multicast ECHO(h,bi,si)
-- • upon receiving ECHO(h,bj,sj) from party Pj,
--   check that bj is a valid Merkle branch for root h and leaf sj, and otherwise discard
-- • upon receiving valid ECHO(h, ·, ·) messages from N − f distinct parties,
--   - interpolate {s′j } from any N − 2f leaves received
--   – recompute Merkle root h′ and if h′ ̸= h then abort
--   – if READY(h) has not yet been sent, multicast READY(h)
-- • upon receiving f + 1 matching READY(h) messages, if READY has not yet been sent, multicast READY(h)
-- • upon receiving 2f + 1 matching READY(h) messages, wait for N − 2f ECHO messages, then decode v
--
-- The functions here are used to turn input into output. The IO is supposed to be handled separately.
--
-- The RBC module is not concerned about the authenticity of the communicators - verify validator's identity
-- before passing messages to RBC!

import Data.Binary (Binary, encode, decode)
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString as BS (length)
import Data.List (sortBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Typeable (Typeable)
import Control.Exception (AssertionFailed(..))
import Control.Monad.Catch
  ( Exception(..)
  , MonadCatch
  , MonadMask
  , handle
  , onException
  , throwM
  , try
  )
import Control.Monad.State
import ErasureCoding (decodeMessage, encodeByteString)
import GHC.Generics (Generic)
import MerkleTree
  ( DecodingError(..)
  , MerkleProof
  , mkMerkleProof
  , validateMerkleProof
  )

type Validator = Int

type Shard = ByteString

type Proof = (MerkleProof, Int)

data RBCState =
  RBCState
    { getValidators :: [Validator]
    , getN :: Int
    , getF :: Int
    , getSelf :: Validator
    , getEcho :: [(Validator, Proof)]
    , getReady :: [(Validator, ByteString)]
    , getReadySent :: Bool
    , getMessage :: ByteString
    }
  deriving (Show)

data In
  = Input ByteString
  | Val Proof
  | Echo Proof
  | Ready ByteString
  deriving (Eq, Generic, Show, Binary, Typeable)

data Out
  = Broadcast [(In, Validator)]
  | Output ByteString
  | StoreMessage (ByteString, Out)
  | StoreEcho (Proof, Validator)
  | StoreReady (ByteString, Validator)
  | None
  deriving (Show, Generic, Binary, Typeable)

data RBCError
  = ErasureCodingError String
  | UnknownValidator String
  | OwnMessageError String
  | ProofDecodingError String
  deriving (Show)

instance Exception RBCError

setup :: Int -> Validator -> RBCState
setup n self =
  RBCState
    { getValidators = [v | v <- [0 .. n - 1], v /= self]
    , getN = n
    , getF = getByzantineToleranceNumber n
    , getSelf = self
    , getEcho = []
    , getReady = []
    , getReadySent = False
    , getMessage = empty
    }

getByzantineToleranceNumber n | n <= 3 = 0
getByzantineToleranceNumber n = n `div` 3

getErasureCodingScheme n = (max (n - 2 * f) 2, n)
  where
    f = getByzantineToleranceNumber n

receive :: (MonadCatch m, MonadIO m) => In -> Validator -> State RBCState (m Out)
receive message validator = do
  state <- get
  if validator >= 0 && validator < getN state
    then if validator == getSelf state && null [x | Input x <- [message]]
           then return . throwM $
                OwnMessageError "Do not process my own messages"
           else receive' message validator
    else return . throwM $
         UnknownValidator $ "Do not know this validator: " ++ show validator

parseInput message state = do
  let (m, n) = getErasureCodingScheme (getN state)
  shards <- fmap (toStrict . encode) <$> encodeByteString m n message
  pure . Broadcast $
    zipWith
      (\v shard -> (Val (mkMerkleProof shards shard, BS.length message), v))
      (getValidators state)
      shards

receive' :: (MonadCatch m, MonadIO m) => In -> Validator -> State RBCState (m Out)
-- • upon input(v) (if Pi = PSender):
--   let {sj} j∈[N] be the blocks of an (N − 2f, N) erasure coding scheme applied to v
--   let h be a Merkle tree root computed over {sj}
--   send VAL(h,bj,sj) to each party Pj, where bj is the jth Merkle tree branch
receive' (Input message) _ = gets (parseInput message)

-- • upon receiving VAL(h,bi,si) from PSender, multicast ECHO(h,bi,si)
receive' (Val proof) _ = do
  state <- get
  return $ return $ Broadcast ((Echo proof, ) <$> getValidators state)

-- • upon receiving ECHO(h,bj,sj) from party Pj,
--   check that bj is a valid Merkle branch for root h and leaf sj, and otherwise discard
receive' (Echo proof) validator = do
  state <- get
  if validator `elem` map fst (getEcho state) -- collect messages from distinct validators
    then return $ return None
    else return $ validateEcho proof validator state

receive' (Ready root) validator = do
  state <- get
  if validator `elem` map fst (getReady state) -- collect messages from distinct validators
    then return $ return None
    else return $ return $ StoreReady (root, validator)

validateEcho (proof, size) validator state = do
  maybeValid <- try $ validateMerkleProof proof
  case maybeValid of
    Left (DecodingError msg) -> throwM $ ProofDecodingError msg
    Right isValid ->
      if isValid
        then return $ StoreEcho ((proof, size), validator)
        else return None

storeEcho :: (MonadIO m, MonadMask m) => Proof -> Validator -> State RBCState (m Out)
storeEcho proof validator = do
  state <- get
  put $ state {getEcho = getEcho state ++ [(validator, proof)]}
  if getReadySent state
    then maybeDecodeMessage
    else countEchoes

markReadySent :: State RBCState ()
markReadySent = do
  state <- get
  put state {getReadySent = True}

-- • upon receiving f + 1 matching READY(h) messages, if READY has not yet been sent, multicast READY(h)
storeReady :: MonadMask m => ByteString -> Validator -> State RBCState (m Out)
storeReady root validator = do
  state <- get
  put state {getReady = getReady state ++ [(validator, root)]}
  if getReadySent state
    then maybeDecodeMessage
    else countReadies

countReadies :: MonadMask m => State RBCState (m Out)
countReadies = do
  state <- get
  let received = Map.fromListWith (++) (map (\(v, r) -> (r, [v])) $ getReady state)
  let matchingSubsets =
        [ subset
        | subset@(_, validators) <- Map.toList received
        , length validators > getF state
        ]
  if length matchingSubsets > 1 || null matchingSubsets
    then return $ return None
    else return $ return $ composeBroadcast (Ready (fst $ head matchingSubsets)) state

countEchoes :: (MonadIO m, MonadMask m) => State RBCState (m Out)
countEchoes = do
  state <- get
    -- • upon receiving valid ECHO(h, ·, ·) messages from N − f distinct parties,
  if length (getEcho state) < getN state - getF state
    then return $ return None
    else interpolateEchoes

-- • upon receiving 2f + 1 matching READY(h) messages, wait for N − 2f ECHO messages, then decode v
maybeDecodeMessage :: MonadMask m => State RBCState (m Out)
maybeDecodeMessage = do
  state <- get
  if length (getEcho state) >= getN state - 2 * getF state &&
     length (getReady state) >= 2 * getF state + 1
    then return $ return $ Output (getMessage state)
    else return $ return None

storeMessage :: ByteString -> State RBCState ()
storeMessage message = do
  state <- get
  put state {getMessage = message}

interpolateEchoes :: (MonadIO m, MonadMask m) => State RBCState (m Out)
interpolateEchoes = do
  state <- get
    -- group echoes by (Merkle root, message size) cause they must match for echoes we interpolate the message from
  let echoes =
        Map.fromListWith
          (++)
          (map
             (\(v, ((_, root, leaf), size)) -> ((root, size), [(v, leaf)]))
             (getEcho state))
    --   - interpolate {s′j } from any N − 2f leaves received
  let (dataShards, totalShards) = getErasureCodingScheme $ getN state
  let subsets =
        [ (key, getShards (sortOn fst subset) 0 (getN state))
        | (key, subset) <- Map.toList echoes
        , length subset >= (totalShards - dataShards)
        ]
  return $ interpolateEchoes' subsets state

getShards :: [(Validator, ByteString)] -> Int -> Int -> [Maybe ByteString]
getShards [] position end = replicate (end - position) Nothing
getShards [(v, leaf)] position end = Just leaf:getShards [] (position + 1) end
getShards shards@((v, leaf):rest) position end =
  if v == position
    then Just leaf : getShards rest (position + 1) end
    else Nothing : getShards shards (position + 1) end

interpolateEchoes' :: (MonadIO m, MonadMask m) => [((ByteString, Int), [Maybe ByteString])] -> RBCState -> m Out
interpolateEchoes' [] _ = return None
interpolateEchoes' [subset] state = interpolateEchoes'' subset state
interpolateEchoes' (subset:rest) state = onException (interpolateEchoes'' subset state) (interpolateEchoes' rest state)

interpolateEchoes'' :: (MonadIO m, MonadMask m) => ((ByteString, Int), [Maybe ByteString]) -> RBCState -> m Out
interpolateEchoes'' ((gotRoot, size), echoes) state = do
    let (m, n) = getErasureCodingScheme $ getN state
    maybeDecoded <- try $ decodeMessage $ decode . fromStrict <$> catMaybes echoes
    case maybeDecoded of
        Left (AssertionFailed _) -> return None
        Right message -> return $ StoreMessage (message, composeBroadcast (Ready gotRoot) state)

composeBroadcast :: In -> RBCState -> Out
composeBroadcast msg state = Broadcast $ (msg, ) <$> getValidators state
