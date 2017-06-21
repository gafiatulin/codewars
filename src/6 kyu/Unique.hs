-- Find the unique number
-- http://www.codewars.com/kata/55f81f9aa51f9b72a200002f/

module Codewars.Kata.Unique where

import Data.Maybe (fromJust)
import Data.List (sort, group, find)

findUnique :: [Int] -> Int
findUnique = head . fromJust . find ((==1) . length) . group . sort
