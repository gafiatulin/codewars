-- The maximum sum value of ranges -- Simple version
-- https://www.codewars.com/kata/583d10c03f02f41462000137

module Kata.MaxSumValRanges (maxSum) where

import qualified Data.Vector as V

maxSum :: [Int] -> [(Int,Int)] -> Int
maxSum arr = maximum . map (\(s, e) -> V.sum . V.slice s (e - s + 1) $ v)
    where v = V.fromList arr
