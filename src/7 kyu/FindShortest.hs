-- Shortest Word
-- http://www.codewars.com/kata/57cebe1dc6fdc20c57000ac9

module FindShortest where

find_shortest :: String -> Integer
find_shortest = fromIntegral . minimum . map length . words
