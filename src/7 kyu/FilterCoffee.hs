-- Filter Coffee
-- http://www.codewars.com/kata/56069d0c4af7f633910000d3/

module Codewars.G964.FilterCoffee where

import Data.List (intercalate, sort)

search :: Int -> [Int] -> String
search budget = intercalate "," . map show . sort . filter (<= budget)
