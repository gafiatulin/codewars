-- Remove the minimum
-- http://www.codewars.com/kata/563cf89eb4747c5fb100001b/

module Codewars.Kata.RemoveSmallest where

import Data.List (delete)

removeSmallest :: [Int] -> [Int]
removeSmallest xs = delete (minimum xs) xs
