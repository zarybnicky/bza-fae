{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Random.Dice
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
-- 
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements an @n@-sided dice and provides sampling from a given
-- integer range.
-- The algorithm uses rejection sampling and attempts to keep the total number
-- of used random bits as close as possible to the information theoretic lower
-- bound of @ln(n) / ln(2)@ (for a range of size @n@).
-- 
-- The implementation exposes streams of random values as conduits, see
-- 'diceRolls' and 'randomRs'. We also provide IO wrappers around these
-- functions, see 'getDiceRolls' and 'getRandomRs'.
-- The conduit interface allows us to use a specific entropy source, which has
-- type 'Producer' 'IO' 'Word'. 
--
-- /Usage:/
-- 
-- If we wanted to use the system-specific entropy source ('systemEntropy') to
-- produce 10 dice rolls of a 6-sided dice (i.e. range [0,5]), we could write:
--
-- > > systemEntropy $$ diceRolls 6 =$= CL.take 10 
-- > [5,1,3,3,0,5,3,2,2,1]
--
-- The function 'testPerformance' yields the actual number of consumed random
-- bits:
-- 
-- > > testPerformance 12 10000
-- > Generated 10000 random samples in range [0,11]
-- > Average number of bits used: 3.5904
-- > Entropy lower bound on the number of required bits: 3.5849625007211565
-- > Performance ratio: 1.0015167520658164
--
-----------------------------------------------------------------------------
module System.Random.Dice( diceRolls
                         , randomRs
                         , getDiceRolls
                         , getRandomRs
                         , testPerformance
                         , systemEntropy
                         )
where
import System.Random.Dice.Internal

