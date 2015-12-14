-- Playing with passphrases
-- http://www.codewars.com/kata/559536379512a64472000053/

module Codewars.Kata.PlayPass where

import Data.Char (toLower, toUpper, digitToInt, intToDigit, ord, chr, isDigit, isLetter)

playPass :: String  -> Int -> String
playPass str shift = reverse . zipWith ($) (cycle [toUpper, id]) . map (f . toLower) $ str
    where f c | isDigit c = intToDigit . (9-) . digitToInt $ c
              | isLetter c = chr . (ord 'a' +) . (`mod` 26) $ (ord c - ord 'a' + shift)
              | otherwise = c
