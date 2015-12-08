-- Scrabble Score
-- http://www.codewars.com/kata/558fa34727c2d274c10000ae

module Codewars.Exercise.Scrabble where

import Codewars.Exercise.Scrabble.Score (dict)
import Data.Char (toUpper)
import Data.Map.Lazy (findWithDefault, fromList)

scrabbleScore :: String -> Int
scrabbleScore str = sum $ map (\c -> findWithDefault 0 (toUpper c) (fromList dict)) str
