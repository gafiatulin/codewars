-- Next Version
-- https://www.codewars.com/kata/56c0ca8c6d88fdb61b000f06

module NextVersion where

import Data.List (intercalate)
import Data.List.Split (splitOn)

nextVersion :: String -> String
nextVersion = intercalate "." . map show . reverse . f 1 . map read . reverse . splitOn "."
  where f 0 xs = xs
        f 1 [x] = [x+1]
        f 1 (9: xs) = 0:(f 1 xs)
        f 1 (x: xs) = (x+1):xs
