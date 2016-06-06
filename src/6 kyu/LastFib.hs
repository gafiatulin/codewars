-- Find last Fibonacci digit [hardcore version]
-- http://www.codewars.com/kata/56b7771481290cc283000f28

module Codewars.Kata.LastFib where

lastFibDigit :: Integer -> Int
lastFibDigit = (`mod` 10) . (fib !!) . fromIntegral . (`mod` 60)
    where fib = 0 : 1 : zipWith (+) fib (tail fib)
