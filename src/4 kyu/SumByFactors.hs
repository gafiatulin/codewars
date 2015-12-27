-- Sum by Factors
-- http://www.codewars.com/kata/54d496788776e49e6b00052f/

module Codewars.Kata.SumByFactors where

import Data.Set (fromList, toList)
import Data.Map.Lazy (fromListWith, toAscList)

sumOfDivided xs = toAscList . fromListWith (+) . concatMap (\n -> map (\d -> (d, n)) . divisors $ n ) $ xs

divisors n | n < 0 = divisors . negate $ n
           | n `elem` [0, 1] = []
           | otherwise = toList . fromList . pFactors $ n
           where pFactors n = facts n (2:[3,5..])
                 facts n (f:fs) | n < f*f = [n]
                                | n `mod` f == 0 = f : facts (n `div` f) (f:fs)
                                | otherwise = facts n fs
