-- The highest profit wins!
-- http://www.codewars.com/kata/559590633066759614000063

module Codewars.Kata.MinMax where

minMax :: (Ord a) => [a] -> (a, a)
minMax xs = (minimum xs, maximum xs)
