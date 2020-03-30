{-# LANGUAGE Rank2Types, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Random.Dice.Internal
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
-- 
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
module System.Random.Dice.Internal
where
import System.Entropy
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import qualified Data.ByteString as B
import Data.Word
import Data.Conduit
import qualified Data.Conduit.List as CL

-- | Converts a number to its base-2 representation (as a list of bits) 
-- and prepends zeros to ensure the minimal size.
integralToBits :: (Integral n,Integral m)
               => Int  -- ^ minimal number of bits @b@
               -> n    -- ^ the number @n@
               -> [m]  -- ^ bit representation of @n@, length >= @b@
integralToBits b x = reverse $ integralToBits' 0 x 
  where
  integralToBits' ns 0 = replicate (b-ns) 0
  integralToBits' ns y = 
    let (a,res) = quotRem y 2 in 
    fromIntegral res : integralToBits' (ns+1) a
    

-- | Convert a list of bits to an integral
bitsToIntegral :: (Integral n) =>[n] -> n
bitsToIntegral = extendIntegralWithBits 0 
 

extendIntegralWithBits :: (Integral n) => n -> [n] -> n
extendIntegralWithBits n = foldr (\c r -> 2*r + c) n . reverse


-- | Upper bound on the number of sides that a random dice can have.
upperBound :: Word64
upperBound = 2^(55 :: Int)


-- | Generates @k@ rolls of an @n@ sided dice.
getDiceRolls :: Int  -- ^ @n:@ number of sides
             -> Int  -- ^ @k:@ number of rolls
             -> IO [Int]
getDiceRolls n len = 
  systemEntropy $$ diceRolls n =$= CL.take len 


-- | Generates a list of random integer values in the specified range.
getRandomRs :: (Int,Int) -- ^ (inclusive) range
         -> Int          -- ^ number of samples
         -> IO [Int]
getRandomRs range len = 
  systemEntropy $$ randomRs range =$= CL.take len


-- | Produces a stream of random integer values in the range @[0,n-1]@, for a
-- given @n <= 2^55@.
-- This conduit needs to be attached to an entropy source such as
-- 'systemEntropy'.
diceRolls :: Int -> Conduit Word8 IO Int
diceRolls n
  | fromIntegral n > upperBound || n <= 0
    = throw $ AssertionFailed "diceRolls: n-sided dice are supported, for 1 <= n < 2^55."
  | n == 1    
    = CL.sourceList [0,0..] 
  | otherwise 
    = dRoll (fromIntegral n) 1 0 =$= CL.map fst


-- | Produces a stream of random integer values within a range.
-- This conduit needs to be attached to an entropy source such as
-- 'systemEntropy'.
randomRs :: (Int,Int)             -- ^ range (inclusive)
         -> Conduit Word8 IO Int
randomRs (low,up) = diceRolls (up-low+1) =$= CL.map (+low)


-- | A source of entropy. By default, we use the 'getEntropy' function from
-- the entropy package, see 'systemEntropy'. 
--
-- /Warning:/ When combining a source of entropy with other conduits, it is
-- important that there is no \"backflow\" due to leftover values that 
-- are being returned to the
-- source from the conduit. This can be done by fusing the conduit with the
-- identity map, e.g: @myEntropySrc $$ Data.Conduit.List.map id =$= myConduit@
-- 
systemEntropy :: Producer IO Word8 
systemEntropy = do
  bytes <- B.unpack `liftM` liftIO  (getEntropy 8)
  forM_ bytes yield
  systemEntropy 



-- | Internal function. Should not be invoked directly. 
dRoll :: Word64 -> Word64 -> Word64 -> Conduit Word8 IO (Int,Int)
dRoll n m r = do
--    | num > len = print ("end:",num,m,r) >> return []
--  | otherwise = do
  let k = ceiling $ (logBase 2 (fromIntegral upperBound) - logBase 2 (fromIntegral m :: Double)) / 8 
  let m' = 2^(8*k) * m
  bits <- (concatMap (integralToBits 8) . B.unpack) 
          `liftM` (if k>0 then liftIO $ getEntropy k else return $ B.pack [])
  let w64 = extendIntegralWithBits r bits
  let q = m' `div` n
  if w64 < n * q 
    then do
      yield (fromIntegral $ w64 `mod` n,k)
      dRoll n q (w64 `div` n)
    else  dRoll n (m' - n*q) (w64 - n*q)


-- | Compute the performance of the algorithm in terms of used random bits
-- versus produced random values.
testPerformance :: Int   -- ^ number of sides of dice
                -> Int   -- ^ number of samples used for computing average.
                -> IO ()
testPerformance n len 
  | fromIntegral n > upperBound
    = throw $ AssertionFailed "dice: range must be within Word64 bounds."
  | otherwise = do
    nbits <- systemEntropy $= dRoll (fromIntegral n) 1 0 
                           $$ CL.take len 
                           >>= return . sum . map snd
    putStrLn $ "Generated " ++ show len 
            ++ " random samples in range [0," ++ show (n-1) ++ "]"
    putStrLn $ "Average number of bits used: " 
            ++ show (8*fromIntegral nbits/ fromIntegral len :: Double)
    let lbound = logBase 2 (fromIntegral n) :: Double
    putStrLn $ "Entropy lower bound on the number of required bits: " 
            ++ show lbound
    putStrLn $ "Performance ratio: " ++ show (((8*fromIntegral nbits 
                                    / fromIntegral len) ::Double) / lbound)


