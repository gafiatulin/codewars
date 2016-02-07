-- Next Palindromic Number.
-- http://www.codewars.com/kata/56a6ce697c05fb4667000029/

module Codewars.Palindromes where

nextPal :: Int -> Int
nextPal n = head . dropWhile (\x -> show x  /= (reverse . show $ x)) $ [n+1, n+2 ..]
