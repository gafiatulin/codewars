-- RLE
-- https://www.codewars.com/kata/578bf2d8daa01a4ee8000046

module Kata.RLE (encode,decode) where

import Data.Char (isNumber)
import Data.List (group, unfoldr)
import Data.Maybe (listToMaybe)
import Data.Tuple (swap)

encode :: String -> String
encode = concatMap (\g -> (show . length $ g) ++ take 1 g) . group

decode :: String -> String
decode = concat . unfoldr f
    where f s = let (n, c:rest) = span isNumber s in if null s then Nothing else fmap (swap . (,) rest . (`replicate` c) . fst) . listToMaybe . reads $ n
