-- Find Count of Most Frequent Item in an Array
-- http://www.codewars.com/kata/56582133c932d8239900002e

module Codewars.Frequency where

import Data.List (sort, group)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)

mostFrequentItemCount :: [Int] -> Int
mostFrequentItemCount [] = 0
mostFrequentItemCount xs = length . maximumBy (comparing length) . group . sort $ xs
