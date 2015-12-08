-- Multiply two numbers a number of times
-- http://www.codewars.com/kata/55252a50de8b4bac00000805

module Codewars.Kata.Multiply where

multiplyBy :: Integer -> Integer -> Int -> [Integer]
multiplyBy x y n = [x * y^k | k <- [1..n]]
