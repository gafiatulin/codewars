-- Highest Scoring Word
-- https://www.codewars.com/kata/57eb8fcdf670e99d9b000272

module High.JorgeVS.Kata where

import Data.Char (ord, toLower)
import Data.Ord (comparing)
import Data.List (maximumBy)

high :: String -> String
high [] = []
high s = maximumBy (comparing (sum . map ((+ (negate . pred . ord $ 'a')) . ord . toLower))) . words $ s
