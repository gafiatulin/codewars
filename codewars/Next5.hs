-- Round to the next 5.
-- http://www.codewars.com/kata/55d1d6d5955ec6365400006d

module Codewars.Kata.Next5 where

roundToNext5 :: Int -> Int
roundToNext5 n | n `mod` 5 == 0 = n
               | otherwise = 5 * (n `div` 5 + 1)
