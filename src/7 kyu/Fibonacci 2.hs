-- Fibonacci
-- https://www.codewars.com/kata/57a1d5ef7cb1f3db590002af

module Fibonacci where

fib :: Int -> Int
fib = (map fibS [0 ..] !!)
   where fibS 1 = 1
         fibS 2 = 1
         fibS n = fib (n-2) + fib (n-1)
