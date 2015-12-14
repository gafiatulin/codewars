-- Exercise in Summing
-- http://www.codewars.com/kata/52cd0d600707d0abcd0003eb/

module Codewars.Summing where

import Data.List (sortBy)

extrimeSum _ [] _ = 0
extrimeSum _ _ 0 = 0
extrimeSum f xs n = sum . take n . sortBy f $ xs

minimumSum :: [Integer] -> Int -> Integer
minimumSum = extrimeSum compare

maximumSum :: [Integer] -> Int -> Integer
maximumSum = extrimeSum (flip compare)
