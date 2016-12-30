-- Frequency sequence
-- https://www.codewars.com/kata/585a033e3a36cdc50a00011c

module Kata (freqSeq) where

import Data.Char (intToDigit)
import Data.List (foldl', intersperse)
import qualified Data.Map.Strict as Map

freqSeq :: String -> Char -> String
freqSeq str = (`intersperse` (f str))
    where f s = let m = foldl' (\acc c -> Map.insertWith (+) c 1 acc) Map.empty s in map (\c -> intToDigit . Map.findWithDefault 0 c $ m) s
