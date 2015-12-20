-- Largest 5 digit number in a series
-- http://www.codewars.com/kata/51675d17e0c1bed195000001

module LargestDigits where

import Data.List (tails)

digit5 :: String -> Int
digit5 = maximum . map (read . take 5) . init . tails
