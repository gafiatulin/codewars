-- Sum of differences between products and LCMs
-- http://www.codewars.com/kata/56e56756404bb1c950000992

module Codewars.Kata.SumDifferencesBetweenProductsAndLCMs where

sumDiffs :: [(Integer, Integer)] -> Integer
sumDiffs = sum . map (\(a, b) -> a * b - lcm a b)