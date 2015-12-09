-- Counting power sets
-- http://www.codewars.com/kata/54381f0b6f032f933c000108/

module PowerSetCounting where

powers :: Num a => [t] -> a
powers = (2^) . length
