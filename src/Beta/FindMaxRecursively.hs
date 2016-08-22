-- Find max recursively
-- https://www.codewars.com/kata/57a8873cbb99449e300000ba

module FindMaxRecursively where

maxR :: [Int] -> Int
maxR = foldr1 m
  where m x y | x <= y = y
              | otherwise = x
