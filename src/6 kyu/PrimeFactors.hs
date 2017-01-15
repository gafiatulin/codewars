-- Prime Factors
-- https://www.codewars.com/kata/542f3d5fd002f86efc00081a

module Haskell.PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n | n <= 1 = []
               | otherwise = facts n (2:[3,5..])

facts n (f:fs) | n < f*f = [n]
               | n `mod` f == 0 = f : facts (n `div` f) (f:fs)
               | otherwise = facts n fs
