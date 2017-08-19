-- Word values
-- https://www.codewars.com/kata/598d91785d4ce3ec4f000018

module Haskell.Codewars.WordValues where

import Data.Char (isAsciiLower)

wordValue :: [String] -> [Int]
wordValue = zipWith (*) [1..] . map (sum . map ((+ a') . fromEnum) . filter isAsciiLower)
    where a' = negate . pred . fromEnum $ 'a'
