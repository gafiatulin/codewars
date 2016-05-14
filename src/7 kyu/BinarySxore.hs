-- Binary sXORe
-- http://www.codewars.com/kata/56d3e702fc231fdf72001779

module Kata.BinarySxore where

sxore :: Integer -> Integer
sxore n | even n && n `mod` 4 == 0 = n
        | even n = succ n
        | n `mod` 4 == 1 = 1
        | otherwise = 0
