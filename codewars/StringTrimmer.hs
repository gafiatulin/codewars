-- Trimming a string
-- http://www.codewars.com/kata/563fb342f47611dae800003c

module Codewars.StringTrimmer where

trim :: String -> Int -> String
trim str n | length str <= 3 && n < length str = take n str ++ "..."
           | n < length str = take (n-3) str ++ "..."
           | otherwise = str
