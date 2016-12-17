-- Don't give me five!
-- https://www.codewars.com/kata/5813d19765d81c592200001a

module Kata where

dontGiveMeFive :: Int -> Int -> Int
dontGiveMeFive start end = sum [1 | x <- [start .. end], all (/='5') . show $ x]
