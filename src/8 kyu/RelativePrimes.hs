-- Relatively Prime Numbers
-- https://www.codewars.com/kata/56b0f5f84de0afafce00004e

module Haskell.Codewars.RelativePrimes where

relativelyPrime :: Integral t => t -> [t] -> [t]
relativelyPrime n = filter ((== 1) . gcd n)
