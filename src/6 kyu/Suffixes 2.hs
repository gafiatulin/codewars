-- String Suffixes
-- http://www.codewars.com/kata/559d34cb2e65e765b90000f0/

module Codewars.Exercise.Suffixes where

import Data.List (tails)

stringSuffix :: String -> Int
stringSuffix s = sum . map (f s) . tails $ s
    where f _ [] = 0
          f (x:xs) (y:ys) | x == y = 1 + f xs ys
                          | otherwise = 0
