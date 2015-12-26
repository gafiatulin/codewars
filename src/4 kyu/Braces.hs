-- Valid Braces
-- http://www.codewars.com/kata/5277c8a221e209d3f6000b56/

module Codewars.Kata.Braces where

import Data.Maybe (fromMaybe, listToMaybe)

validBraces :: String -> Bool
validBraces = f []
    where  g '{' = '}'
           g '[' = ']'
           g '(' = ')'
           g x = ' ' 
           f [] [] = True
           f _  [] = False
           f l (x:xs) | x `elem` "({[" = f (x:l) xs
                      | otherwise = if (==x) . g . fromMaybe ' ' . listToMaybe $ l then f (tail l) xs else False
