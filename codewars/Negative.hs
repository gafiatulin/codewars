-- Return Negative
-- http://www.codewars.com/kata/55685cd7ad70877c23000102

module Codewars.Kata.Negative where

makeNegative :: (Num a, Ord a) => a -> a
makeNegative n | n > 0 = -n
               | otherwise = n
