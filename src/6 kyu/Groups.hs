-- Checking Groups
-- http://www.codewars.com/kata/54b80308488cb6cd31000161/
-- Duplicate of http://www.codewars.com/kata/5277c8a221e209d3f6000b56/

module Codewars.Kata.Groups where

import Data.Maybe (fromMaybe, listToMaybe)

groupCheck :: String -> Bool
groupCheck = f []
    where  g '{' = '}'
           g '[' = ']'
           g '(' = ')'
           g x = ' ' 
           f [] [] = True
           f _  [] = False
           f l (x:xs) | x `elem` "({[" = f (x:l) xs
                      | otherwise = if (==x) . g . fromMaybe ' ' . listToMaybe $ l then f (tail l) xs else False
