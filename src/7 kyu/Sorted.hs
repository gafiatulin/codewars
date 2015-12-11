-- Return a sorted list of objects
-- http://www.codewars.com/kata/52705ed65de62b733f000064

module Sorted where

import Data.List (sortBy)
import Data.Ord (comparing)

sortList :: Ord b => (a -> b) -> [a] -> [a]
sortList = sortBy . comparing
