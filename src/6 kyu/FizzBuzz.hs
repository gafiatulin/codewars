-- Fizz / Buzz
-- http://www.codewars.com/kata/51dda84f91f5b5608b0004cc/

module FizzBuzz where

fizzbuzz :: Int -> [Int]
fizzbuzz n = [f 3 - f 15, f 5 - f 15, f 15]
    where f = div (n-1)
