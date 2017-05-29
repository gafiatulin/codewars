-- Simple Fun #299: Look And Say And Sum
-- https://www.codewars.com/kata/5922828c80a27c049c000078

module Haskell.Codewars.LookAndSayAndSum where

import Data.List (group)
import Data.Char (digitToInt)

lookAndSaySum :: Int -> Int
lookAndSaySum = let lookAndSay = 1 : map (read . concatMap (\s -> show (length s) ++ [head s]) . group . show) lookAndSay in  sum . map digitToInt . show . (lookAndSay !!) . pred
