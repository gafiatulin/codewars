-- Adjacent pairs in a string
-- http://www.codewars.com/kata/5245a9138ca049e9a10007b8/

module AdjacentPairs where

import Data.List (group)
import Data.Char (toLower)

countAdjacentPairs :: String -> Int
countAdjacentPairs = length . filter ((>1) . length) . group . words . map toLower
