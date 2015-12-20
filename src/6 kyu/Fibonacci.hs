-- Memoized Fibonacci
-- http://www.codewars.com/kata/529adbf7533b761c560004e5/

module Fibonacci where

fibonacci :: Int -> Integer
fibonacci = (map fib [0 ..] !!)
    where fib 0 = 0
          fib 1 = 1
          fib n = fibonacci (n-2) + fibonacci (n-1)
