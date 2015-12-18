-- Scrabble best word
-- http://www.codewars.com/kata/563f960e3c73813942000015/

module Codewars.Kata.Scrabble where

import Data.Char (ord)
import Data.List (maximumBy)
import Control.Arrow ((&&&))

getBestWord :: [Int] -> [String] -> Int
getBestWord ps ws = (\(_, _, i) -> i) . maximumBy cmp . zipWith (\i (l, s) -> (s, l, i)) [0..] . map (length &&& score) $ ws
    where score = sum . map (\c -> ps !! (ord c - ord 'A'))
          cmp (s1, l1, i1) (s2, l2, i2) = case compare s1 s2 of
              EQ -> case compare l2 l1 of EQ -> compare i2 i1
                                          o1  -> o1
              o2  -> o2
