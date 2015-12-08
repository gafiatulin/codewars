-- Anagram Detection
-- http://www.codewars.com/kata/529eef7a9194e0cbc1000255

module Codewars.Anagram where

import Data.Char (toLower)
import Data.List (sort)

isAnagramOf :: String -> String -> Bool
isAnagramOf xs ys = (sort . map toLower) xs == (sort . map toLower) ys
