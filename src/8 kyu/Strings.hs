-- Regex count lowercase letters
-- http://www.codewars.com/kata/56a946cd7bd95ccab2000055/

module Codewars.Strings where

import Data.Char (isLower)

lowercaseCount :: String -> Int
lowercaseCount = length . filter (isLower)
