-- Unique Sum
-- http://www.codewars.com/kata/56b1eb19247c01493a000065/

module Codewars.Arrays where

import Data.List (nub)

uniqueSum :: [Int] -> Int
uniqueSum = sum . nub
