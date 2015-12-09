-- Alternate Square Sum
-- http://www.codewars.com/kata/559d7951ce5e0da654000073/

module Codewars.Exercises.Alternate where

import Data.Maybe (listToMaybe)

alternateSqSum :: [Integer] -> Maybe Integer
alternateSqSum [] = Nothing
alternateSqSum seq = Just (sum $ zipWith (^) seq (cycle [1, 2]))
