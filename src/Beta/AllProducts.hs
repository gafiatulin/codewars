-- Get all possible products of two lists of numbers
-- https://www.codewars.com/kata/57da6a507691c30926000042

module AllProducts where

import Control.Applicative(liftA2)

getProducts :: [Integer] -> [Integer] -> [Integer]
getProducts = flip $ liftA2 (*)
