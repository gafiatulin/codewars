-- Sum of list of Integer Just's and Nothing's
-- https://www.codewars.com/kata/57da7f901b5ff148ad00030d

module SumOfJustsAndNothings where

import Data.Maybe (catMaybes)

sumJusts :: [Maybe Integer] -> Maybe Integer
sumJusts = Just . sum . catMaybes
