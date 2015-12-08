-- Count consonants
-- http://www.codewars.com/kata/564e7fc20f0b53eb02000106

module Codewars.ConsonantCounter where

import Data.Char (toLower, isLetter)

consonantCount :: String -> Int
consonantCount = length . filter (\c -> c `notElem` "aeiou" && isLetter c) . map toLower
