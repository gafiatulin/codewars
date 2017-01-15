-- Series of integers from 0 to n
-- https://www.codewars.com/kata/5841f4fb673ea2a2ae000111

module Kata (generateIntegers) where

generateIntegers :: Int -> [Int]
generateIntegers = enumFromTo 0
