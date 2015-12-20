-- Product of consecutive Fib numbers
-- http://www.codewars.com/kata/5541f58a944b85ce6d00006a/

module Codewars.Kata.Fib where

productFib :: Integer -> (Integer, Integer, Bool)
productFib n = (\(a, b) -> (a, b, a*b == n)) . head . dropWhile (\(a, b) -> a*b < n) . zip fib $ tail fib
    where fib = 1:1: zipWith (+) fib (tail fib)
