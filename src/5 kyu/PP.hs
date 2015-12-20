-- What's a Perfect Power anyway?
-- http://www.codewars.com/kata/54d4c8b08776e4ad92000835/

module Codewars.Kata.PP (isPP) where

import Data.Maybe(listToMaybe)

isPP :: Integer -> Maybe (Integer, Integer)
isPP i = listToMaybe [ (fromIntegral m, fromIntegral k) | m <- ms, k <- [2.. ceiling (logBase (fromIntegral m) (fromIntegral i))], m^k == fromIntegral i]
    where maxM = floor . sqrt . fromIntegral $ i
          ms = if odd i then [3,5..maxM] else [2,4..maxM]
