-- Adding ordinal indicator suffixes to numbers
-- http://www.codewars.com/kata/52dca71390c32d8fb900002b/

module Codewars.Suffixes where

numberToOrdinal :: Int -> String
numberToOrdinal n | n == 0 = "0"
                  | n `mod` 100 < 20 = ordS 100
                  | otherwise = ordS 10
                  where ordS x = case n `mod` x of 
                          1 -> show n ++ "st"
                          2 -> show n ++ "nd"
                          3 -> show n ++ "rd"
                          _ -> show n ++ "th"
