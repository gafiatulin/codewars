-- Find all possible number combos that sum to a number
-- http://www.codewars.com/kata/555b1890a75b930e63000023/

module Codewars.Kata.Combos where

combos :: Int -> [[Int]]
combos = let f x y = [y] : [k:l | k <- [x .. y `div` 2], l <- f k (y-k)] in f 1
