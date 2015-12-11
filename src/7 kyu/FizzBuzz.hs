-- Fizz Buzz
-- http://www.codewars.com/kata/5300901726d12b80e8000498/

module FizzBuzz(fizzbuzz) where

fizzbuzz :: Int -> [String]
fizzbuzz n = map f [1..n]
    where f x | x `mod` 15 == 0 = "FizzBuzz"
              | x `mod` 5 == 0 = "Buzz"
              | x `mod` 3 == 0 = "Fizz"
              | otherwise = show x
