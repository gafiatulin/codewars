-- Collatz Conjecture Length
-- http://www.codewars.com/kata/54fb963d3fe32351f2000102/

module Codewars.Kata.Collatz where

import Data.List (unfoldr)

collatz = length . unfoldr f
    where f n | n == 0 = Nothing
              | n == 1 = Just (1, 0)
              | even n = Just (n, n `div` 2)
              | otherwise = Just (n , (n * 3) + 1)
