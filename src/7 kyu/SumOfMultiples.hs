-- Sum of all the multiples of 3 or 5
-- https://www.codewars.com/kata/57f36495c0bb25ecf50000e7

module SumOfMultiples where

findSum n = f 5 n + f 3 n - f 15 n
    where f d n = let a = (n `div` d) in (a * d * succ a) `div` 2
