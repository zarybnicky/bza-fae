module Main
where
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Maybe
import qualified Data.List as L
import Control.Monad

import Data.ByteString.Lazy( ByteString )
import qualified Data.ByteString.Lazy as B

import System.Random.Dice.Internal

instance Arbitrary ByteString where
    arbitrary   = fmap B.pack arbitrary

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "convertingToFromBits" propConvertingToFromBits
       ] mempty

propConvertingToFromBits bstr = 
  let ws = B.unpack bstr in
  let ws' = map (integralToBits 8) ws in
  ws == map bitsToIntegral ws'
  
  
