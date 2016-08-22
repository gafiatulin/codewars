-- Get list sum recursively
-- https://www.codewars.com/kata/57a84137cf1fa5f9f80000d6

module SumRecursively where

sumR :: [Int] -> Int
sumR [] = 0
sumR (x: xs) = x + sumR xs
