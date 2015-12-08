-- Case swapping
-- http://www.codewars.com/kata/5590961e6620c0825000008f

module Codewars.Kata.CaseSwapping where

import Data.Char (isUpper, toLower, toUpper)

swap :: String -> String
swap = map swapChar
    where swapChar c | isUpper c = toLower c
                     | otherwise = toUpper c
