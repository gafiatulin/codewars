-- Find the Mine!
-- http://www.codewars.com/kata/528d9adf0e03778b9e00067e/

module MineLocation where

import Data.Array
import Data.Maybe (listToMaybe)

mineLocation :: Array (Int, Int) Int -> Maybe (Int, Int)
mineLocation a = listToMaybe [(i, j) | i <- range (0, fst . snd . bounds $ a), j <- range (0, snd . snd . bounds $ a), a!(i, j) == 1]
