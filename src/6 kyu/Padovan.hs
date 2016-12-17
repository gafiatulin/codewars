-- Padovan numbers
-- https://www.codewars.com/kata/5803ee0ed5438edcc9000087

module Padovan where

padovan :: Int -> Int
padovan = let p = 1:1:1:zipWith (+) p (tail p) in (!!) p
