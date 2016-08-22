-- Make all numbers negative
-- https://www.codewars.com/kata/57a20510cf1fa5bfc400095f

module MakeAllNumbersNegative where

makeNegative :: [Int] -> [Int]
makeNegative = map (negate . abs)
