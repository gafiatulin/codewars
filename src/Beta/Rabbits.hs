-- Fibonacci Rabbits
-- http://www.codewars.com/kata/5559e4e4bbb3925164000125/

module Codewars.Kata.Rabbits where

fibRabbits :: Int -> Integer -> Integer 
fibRabbits n b = fib !! n
    where fib = 0:1:zipWith (\ x y -> b*x + y) fib (tail fib)
