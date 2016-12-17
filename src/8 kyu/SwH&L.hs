-- Sum without highest and lowest number
-- https://www.codewars.com/kata/576b93db1129fcf2200001e6

module Kata where

import Data.List (sort)

sumArray :: Maybe [Int] -> Int
sumArray Nothing = 0
sumArray (Just []) = 0
sumArray (Just [_]) = 0
sumArray (Just xs) = sum . init . tail . sort $ xs
