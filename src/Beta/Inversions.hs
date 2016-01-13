-- Calculate number of inversions in array
-- http://www.codewars.com/kata/537529f42993de0e0b00181f/

module Codewars.Exercise.Inversions where

countInversions :: Ord a => [a] -> Int
countInversions [] = 0
countInversions (x:xs) = length (filter (<x) xs) + countInversions xs
