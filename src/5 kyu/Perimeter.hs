-- Perimeter of squares in a rectangle
-- http://www.codewars.com/kata/559a28007caad2ac4e000083/

module Codewars.Kata.Perimeter where

perimeter :: Integer -> Integer
perimeter = (*4) . pred . (fib !!) . (+2) . fromIntegral
    where fib = 1:1: zipWith (+) fib (tail fib)
