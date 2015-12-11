-- Remove Duplicates
-- http://www.codewars.com/kata/53e30ec0116393fe1a00060b

module RemoveDuplicates where

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x: xs) = x : unique (filter (x/=) xs)
