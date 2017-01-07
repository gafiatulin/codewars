-- Character with longest repetition
-- https://www.codewars.com/kata/586d6cefbcc21eed7a001155

module Kata(longestRepetition) where

import Data.Ord (comparing)
import Control.Arrow ((&&&))
import Data.List (group, maximumBy)

longestRepetition :: String -> Maybe (Char, Int)
longestRepetition [] = Nothing
longestRepetition s = Just . maximumBy (comparing snd) . map (head &&& length) . group $ s
