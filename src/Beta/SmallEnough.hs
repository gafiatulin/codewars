-- Small enough? - Beginner
-- https://www.codewars.com/kata/57cc981a58da9e302a000214

module Kata where

smallEnough :: [Int] -> Int -> Bool
smallEnough xs v = all (<= v) xs
