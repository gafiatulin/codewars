-- SumFibs
-- http://www.codewars.com/kata/56662e268c0797cece0000bb

module Codewars.Fibonacci where

sumFibs :: Int -> Integer
sumFibs = sum . filter even . (`take` fibs) . succ
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
