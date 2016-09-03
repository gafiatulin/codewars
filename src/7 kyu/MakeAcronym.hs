-- Make acronym
-- https://www.codewars.com/kata/57a60bad72292d3e93000a5a

module MakeAcronym where

import Data.Char (toUpper)

toAcronym :: String -> String
toAcronym = map (toUpper . head) . words
