-- Sum of numbers from 0 to N
-- http://www.codewars.com/kata/56e9e4f516bcaa8d4f001763

module Sequence.JorgeVS.Kata where

import Data.List (intercalate)

sequenceSum :: Int -> String
sequenceSum n | n < 0 = show n ++ " < 0"
              | n == 0 = "0 = 0"
              | otherwise = (intercalate "+" . map show $ [0..n]) ++ " = " ++ show (n*(n+1) `div` 2)
