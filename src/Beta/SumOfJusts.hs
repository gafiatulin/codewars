-- Sum of list of Integer Just's
-- https://www.codewars.com/kata/57da7ca63150b02e6400026b

module SumOfJusts where

import Data.Maybe (catMaybes, listToMaybe)

sumJusts :: [Maybe Integer] -> Maybe Integer
sumJusts = listToMaybe . (:[]) . sum . catMaybes
