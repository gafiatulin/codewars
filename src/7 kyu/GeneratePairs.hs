-- Pairs of integers from 0 to n
-- https://www.codewars.com/kata/588e27b7d1140d31cb000060

module Kata (generatePairs) where

generatePairs :: Int -> [[Int]]
generatePairs n = [[x, y] | x <- [0..n], y <- [x..n]]
