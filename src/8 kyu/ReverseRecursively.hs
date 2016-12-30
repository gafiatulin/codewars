-- Reverse list recursively
-- https://www.codewars.com/kata/57a883cfbb9944a97c000088

module ReverseRecursively where

revR :: [Int] -> [Int]
revR = foldl (flip (:)) []
