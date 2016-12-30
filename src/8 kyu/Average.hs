-- Calculate average
-- https://www.codewars.com/kata/57a2013acf1fa5bfc4000921

module Average where

import Data.List (genericLength)

avg :: [Float] -> Float
avg l = sum l / genericLength l
