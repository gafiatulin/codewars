-- Remove duplicates from list
-- https://www.codewars.com/kata/57a5b0dfcf1fa526bb000118

module RemoveDuplicates  where
import Data.List (nub)

distinct :: [Int] -> [Int]
distinct = nub
