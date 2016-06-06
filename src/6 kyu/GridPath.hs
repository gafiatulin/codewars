-- Paths in the Grid
-- http://www.codewars.com/kata/56a127b14d9687bba200004d

module Kata.GridPath where

numberOfRoutes :: Int -> Int -> Integer
numberOfRoutes m n = product [succ b .. a + b] `div` product [1 .. a]
    where a = fromIntegral (min m n)
          b = fromIntegral (max m n)
