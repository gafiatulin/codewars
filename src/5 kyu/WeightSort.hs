-- Weight for weight
-- http://www.codewars.com/kata/55c6126177c9441a570000cc/

module Codewars.G964.WeightSort where

import Data.Char (digitToInt)
import Data.List(sortBy)

orderWeight :: String -> String
orderWeight = unwords . sortBy cmp . words
    where weight = sum . map digitToInt 
          cmp s1 s2 = case compare (weight s1) (weight s2) of EQ -> compare s1 s2
                                                              x  -> x
