-- Calculate mean and concatenate string
-- https://www.codewars.com/kata/56f7493f5d7c12d1690000b6

module Kata (mean) where

import Data.Char (isDigit, digitToInt)
import Control.Arrow (first, (&&&))
import Data.List (partition, genericLength)

mean :: String -> (Double, String)
mean = first (uncurry (/) . ( fromIntegral . sum . map digitToInt &&& genericLength)) . partition isDigit
