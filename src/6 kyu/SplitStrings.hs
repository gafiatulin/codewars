-- Split Strings
-- https://www.codewars.com/kata/515de9ae9dcfc28eb6000001

module Codewars.Kata.SplitStrings where

import Data.List.Split (chunksOf)

solution :: String -> [String]
solution = let f xs = if (==2) . length $ xs then xs else xs ++ ['_'] in map f . chunksOf 2
