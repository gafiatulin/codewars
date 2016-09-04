-- Coding with Squared Strings
-- https://www.codewars.com/kata/56fcc393c5957c666900024d

module Codewars.G964.CoddecSqStrings(code, decode) where

import Data.Char (chr)
import Data.List (genericLength, intercalate, transpose)
import Data.List.Split (chunksOf)

specialChar = chr 11

rot90Counter = reverse . transpose . lines

complete :: String -> (String, Int)
complete str = (s, n)
    where s = (str ++) . replicate (n^2 - genericLength str) $ specialChar
          n = ceiling . sqrt . genericLength $ str

code :: String -> String
code str = intercalate "\n" . map reverse . transpose . chunksOf n $ s
    where (s, n) = complete str

decode :: String -> String
decode = takeWhile (/= specialChar) . concat . rot90Counter
