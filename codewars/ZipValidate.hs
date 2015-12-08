-- Russian postal code checker
-- http://www.codewars.com/kata/552e45cc30b0dbd01100001a

module Codewars.Kata.ZipValidate where

import Data.Char (isNumber)

zipValidate :: String -> Bool
zipValidate (x:xs) | length xs == 5 && elem x "12346"  && all isNumber xs = True
                   | otherwise = False
