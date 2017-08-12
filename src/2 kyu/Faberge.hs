-- Fabergè Easter Eggs crush test
-- http://www.codewars.com/kata/54cb771c9b30e8b5250011d4/

module Faberge where

heigth n m | n > m = heigth m m
           | otherwise = f 0 1 1
           where f a b c | c > n = a
                         | otherwise = f (a + d) d (c + 1)
                         where d = b * (m - c + 1) `div` c
