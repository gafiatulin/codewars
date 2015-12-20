-- Is my friend cheating?
-- http://www.codewars.com/kata/5547cc7dcad755e480000004/solutions/haskell

module Codewars.Kata.RemovNB where

import Data.Maybe (mapMaybe)

removNb :: Integer-> [(Integer, Integer)]
removNb n = mapMaybe f [1..n]
    where f x = (\(d, m) -> if m == 0 && d <= n then Just (x, d) else Nothing) ((n * (n+1) `div` 2 - x) `divMod` (x+1))
