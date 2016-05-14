-- No zeros for heros
-- http://www.codewars.com/kata/570a6a46455d08ff8d001002

module Codewars.G964.NoZeros where

noBoringZeros :: Int -> Int
noBoringZeros 0 = 0
noBoringZeros n | n `mod` 10 == 0 = noBoringZeros $ n `div` 10
                | otherwise = n
