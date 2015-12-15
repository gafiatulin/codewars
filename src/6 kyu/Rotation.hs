-- Calculate String Rotation
-- http://www.codewars.com/kata/5596f6e9529e9ab6fb000014/

module Codewars.Kata.Rotation where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

shiftedDiff :: String -> String -> Int
shiftedDiff a b = fromMaybe (-1) . elemIndex True . map (\shift -> (==b) . rotate a $ shift) $ [0..length a -1]
    where rotate str shift = drop (length str - shift `mod` length str) str ++ take (length str - shift `mod` length str) str
