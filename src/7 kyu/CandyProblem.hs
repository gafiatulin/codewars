-- Candy problem
-- http://www.codewars.com/kata/55466644b5d240d1d70000ba/

module CandyProblem where

candies :: [Int] -> Int
candies [] = -1
candies [_] = -1
candies xs = sum $ map (m-) xs where m = maximum xs
