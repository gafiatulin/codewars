-- You're a square!
-- http://www.codewars.com/kata/54c27a33fb7da0db0100040e

module Codewars.Kata.Square where

isSquare :: Integral n => n -> Bool
isSquare n | n == 0 = True
           | n > 0 = let sqrt' = round $ sqrt $ fromIntegral n in sqrt' ^ 2 == n
           | otherwise   = False
