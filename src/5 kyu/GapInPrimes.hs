-- Gap in Primes
-- http://www.codewars.com/kata/561e9c843a2ef5a40c0000a4/

module Codewars.G964.GapInPrimes where

import Data.List (find)
import Control.Arrow ((&&&))

gap :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
gap g m n = find (\(a, b) -> b-a == g) . uncurry zip . (id &&& drop 1) . filter isPrime $ [m..n]
    where isPrime n = all (\d -> n `mod` d  /= 0) [2 .. floor . sqrt . fromIntegral $ n]
