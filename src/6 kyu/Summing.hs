-- Sum of many ints
-- http://www.codewars.com/kata/54c2fc0552791928c9000517/

module Codewars.Kata.Summing where

f :: Integer -> Integer -> Integer
f n m = ((n `div` m)*(m-1)*m + (n `mod` m)*((n `mod` m) + 1)) `div` 2
