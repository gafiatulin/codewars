-- Find the lucky numbers
-- https://www.codewars.com/kata/580435ab150cca22650001fb

module Haskell.Codewars.LuckyNumbers where

filterLucky :: [Int] -> [Int]
filterLucky = filter (elem '7' . show)
