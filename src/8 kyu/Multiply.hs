-- Multiply the number
-- http://www.codewars.com/kata/5708f682c69b48047b000e07

module Codewars.Kata.Multiply where

multiply :: Integer -> Integer
multiply n = n * 5 ^ (length . show . abs $ n)