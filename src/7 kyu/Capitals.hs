-- Find the capitals
-- http://www.codewars.com/kata/539ee3b6757843632d00026b

module Codewars.Kata.Capitals where

import Data.List (findIndices)
import Data.Char (isUpper)

capitals :: String -> [Int]
capitals = findIndices isUpper
