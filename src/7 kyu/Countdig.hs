-- Count the Digit
-- http://www.codewars.com/kata/566fc12495810954b1000030/

module Codewars.G964.Countdig where

import Data.List (elemIndices)
import Data.Char (intToDigit)

nbDig :: Int -> Int -> Int
nbDig n d = sum . map (length . elemIndices (intToDigit d) . show . (^2)) $ [0..n]
