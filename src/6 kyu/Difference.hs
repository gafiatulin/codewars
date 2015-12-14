-- Array.diff
-- http://www.codewars.com/kata/523f5d21c841566fde000009/

module Difference where

difference :: Eq a => [a] -> [a] -> [a]
difference [] _ = []
difference a [] = a
difference a (b:bs) = difference (filter (b/=) a) bs
