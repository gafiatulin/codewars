-- Averages of numbers
-- https://www.codewars.com/kata/57d2807295497e652b000139

module Kata where

averages :: Maybe [Double] -> [Double]
averages = maybe [] (\xs -> zipWith (\a b -> (a+b)/2) xs (tail xs))
