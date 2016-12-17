-- Char Code Calculation
-- https://www.codewars.com/kata/57f75cc397d62fc93d000059

module Kata where

import Data.Char (ord, digitToInt)
import Control.Arrow ((&&&))

calc :: String -> Int
calc = uncurry (-) . ( sum . map digitToInt &&& sum . map (digitToInt . f) ) . concatMap (show . ord)
  where f '7' = '1'
        f x = x
