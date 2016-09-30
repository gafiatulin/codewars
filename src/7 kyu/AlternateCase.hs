-- Alternate case
-- https://www.codewars.com/kata/57a62154cf1fa5b25200031e

module AlternateCase where

import Data.Char (isLower, toLower, toUpper)

alternateCase :: String -> String
alternateCase = map (\x -> if isLower x then toUpper x else toLower x)
