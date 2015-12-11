-- Combinations
-- http://www.codewars.com/kata/54492291ec342c4a440008c5/

module Combinations where

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = [ x:c | c <- combinations (n-1) xs] ++ (combinations n xs)
