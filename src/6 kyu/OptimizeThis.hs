-- Micro Optimization: Digit Sum
-- https://www.codewars.com/kata/59a2af923203e8220b00008f

module OptimizeThis where

import Data.Char (digitToInt)

digitSum :: Integer -> Integer
digitSum = fromIntegral . sum . map digitToInt . show
