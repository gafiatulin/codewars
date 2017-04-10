-- Drying Potatoes
-- http://www.codewars.com/kata/58ce8725c835848ad6000007

module Codewars.G964.Potatoes where

potatoes :: Int -> Int -> Int -> Int
potatoes p0 w0 p1 = w0 * (100 - p0) `div` (100 - p1)
