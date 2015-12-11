-- Find the middle element
-- http://www.codewars.com/kata/545a4c5a61aa4c6916000755

module Codewars.Kata.Middle where

gimme :: Ord a => (a, a, a) -> Int
gimme (a, b, c) | b <= a && a <= c || c <= a && a <= b = 0
                | a <= b && b <= c || c <= b && b <= a = 1
                | a <= c && c <= b || b <= c && c <= a = 2
