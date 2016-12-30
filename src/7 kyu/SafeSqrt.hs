-- SafeSqrt
-- https://www.codewars.com/kata/57ecdd3d2559761b60000403

module SafeSqrt where

safeSqrt x | x >= 0 = show $ sqrt x
           | otherwise = "invalid input"
