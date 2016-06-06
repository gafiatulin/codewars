-- Sum of positive
-- http://www.codewars.com/kata/5715eaedb436cf5606000381

module Codewars.Arrays where

positiveSum :: [Int] -> Int
positiveSum = sum . filter (> 0)
