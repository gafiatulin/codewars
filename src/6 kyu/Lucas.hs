-- Lucas numbers
-- http://www.codewars.com/kata/55a7de09273f6652b200002e/

module Codewars.Exercise.Lucas where

lucasnum :: Int -> Integer
lucasnum n = (if (n<0) then (-1)^n' else 1) * (fibS n' + 2 * fibS (n'-1))
    where n' = abs n
          fib = (map fibS [0 ..] !!)
          fibS 0 = 0
          fibS 1 = 1
          fibS k | k > 0 = fib (k-2) + fib (k-1)
                 | otherwise = (-1)^(k+1) * fibS (abs k)
