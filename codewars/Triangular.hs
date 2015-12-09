-- Triangular Treasure
-- http://www.codewars.com/kata/525e5a1cb735154b320002c8/

module Triangular where

triangular :: Integer -> Integer
triangular n | n > 0 = n * (n + 1) `div` 2
             | otherwise = 0
