-- Circumference Pathing
-- http://www.codewars.com/kata/54c26ee86fd2903443001394/

module Codewars.Kata.Circumference where

circPath :: (Ord a, Num a) => a -> a -> a -> a
circPath s t c | abs a <= abs b = a
               | otherwise = b
               where a = t - s
                     b = signum a * (abs a - c)
