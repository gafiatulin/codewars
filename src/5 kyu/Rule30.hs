-- Rule 30
-- http://www.codewars.com/kata/5581e52ac76ffdea700000c1/

module Codewars.Kata.Rule30 where

rule30 :: [Int] -> Int -> [Int]
rule30 cells n | n <= 0 = cells
               | otherwise = rule30 g (n-1)
               where g = zipWith3 r (0:normCells) normCells ((++[0]) . tail $ normCells)
                     normCells = 0 : map f cells ++ [0]
                     f 1 = 1
                     f _ = 0
                     r 0 0 1 = 1
                     r 0 1 0 = 1
                     r 0 1 1 = 1
                     r 1 0 0 = 1
                     r _ _ _ = 0
