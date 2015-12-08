-- Largest Elements
-- http://www.codewars.com/kata/53d32bea2f2a21f666000256

module CodeWars.Largest (largest) where

import Data.List (sort)

largest :: Ord a => Int -> [a] -> [a]
largest n xs = drop (length xs - n) (sort xs)
