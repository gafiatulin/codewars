-- Count of positives / sum of negatives
-- https://www.codewars.com/kata/576bb71bbbcf0951d5000044

module Kata where

countPositivesSumNegatives :: Maybe [Int] -> [Int]
countPositivesSumNegatives Nothing = []
countPositivesSumNegatives (Just []) = []
countPositivesSumNegatives (Just xs) = [pl, ns]
  where pl = length . filter (>0) $ xs
        ns = sum . filter(<0) $ xs
