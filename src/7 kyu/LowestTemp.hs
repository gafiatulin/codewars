-- Temperature analysis I
-- https://www.codewars.com/kata/588e0f11b7b4a5b373000041

module Kata (lowestTemp) where

import Data.List (sort)
import Data.Maybe (listToMaybe)

lowestTemp :: String -> Maybe Int
lowestTemp = listToMaybe . sort . map read . words
