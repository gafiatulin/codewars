-- How many are smaller than me?
-- http://www.codewars.com/kata/56a1c074f87bc2201200002e/

module Codewars.Kata.Smaller where

smaller :: Ord a => [a] -> [Int]
smaller [] = []
smaller (x:xs) = foldr (\ v -> if v < x then succ else id) 0 xs : smaller xs
