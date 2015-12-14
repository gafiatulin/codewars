-- N-th Fibonacci
-- http://www.codewars.com/kata/522551eee9abb932420004a0/

module Fib where

fib :: Int -> Int
fib = (map fibS [0 ..] !!)
   where fibS 1 = 0
         fibS 2 = 1
         fibS n = fib (n-2) + fib (n-1)
