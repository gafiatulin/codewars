-- First-Class Function Factory
-- http://www.codewars.com/kata/563f879ecbb8fcab31000041

module Kata.FirstClassFunctionFactory where

factory :: Int -> [Int] -> [Int]
factory x = map (*x)
