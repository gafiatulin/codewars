-- Find Fibonacci last digit
-- http://www.codewars.com/kata/56b7251b81290caf76000978/

module Codewars.Numbers where

getLastDigit :: Int -> Int
getLastDigit = (`mod` 10) . (fib !!) . (`mod` 60) . pred
    where fib = 1:1: zipWith (+) fib (tail fib)
