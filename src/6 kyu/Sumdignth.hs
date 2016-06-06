-- Reach Me and Sum my Digits
-- http://www.codewars.com/kata/55ffb44050558fdb200000a4

module Codewars.G964.Sumdignth where

import Data.Char (digitToInt)

sumDigNthTerm :: Int -> [Int] -> Int -> Int
sumDigNthTerm initval patternl nthterm = sum . map digitToInt . show . (!! pred nthterm) . scanl (+) initval . cycle $ patternl
