-- Fibonacci, Tribonacci and friends
-- http://www.codewars.com/kata/556e0fccc392c527f20000c5/

module Xbonacci where

xbonacci :: Num a => [a] -> Int -> [a]
xbonacci as n = map xbonaccis [0..n-1]
    where xbonaccis = (map xbS [0..] !!)
          xbS n | n < length as = as !! n
                | otherwise = sum . map xbonaccis $ [n - length as .. n-1]
