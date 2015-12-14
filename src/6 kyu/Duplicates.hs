-- Counting Duplicates
-- http://www.codewars.com/kata/54bf1c2cd5b56cc47f0007a1/

module Codwars.Kata.Duplicates where

import Data.List (sort, group)
import Data.Char (toLower)

duplicateCount :: String -> Int
duplicateCount = length . filter ((>1) . length ) . group . sort . map toLower
