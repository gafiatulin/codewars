-- Largest product in a series
-- http://www.codewars.com/kata/529872bdd0f550a06b00026e/

module LargestProduct where

import Data.List (tails)
import Data.Char (digitToInt)

greatestProduct :: String -> Int
greatestProduct = maximum . map product . filter ((==5) . length) . map (map digitToInt . take 5) . init . tails
