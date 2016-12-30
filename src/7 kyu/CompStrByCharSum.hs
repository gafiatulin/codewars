-- Compare Strings by Sum of Chars
-- https://www.codewars.com/kata/576bb3c4b1abc497ec000065

module Kata where

import Data.Char (ord, isLetter, toUpper)

compare' :: Maybe String -> Maybe String -> Bool
compare' s1 s2 = (f s1) == (f s2)
  where f Nothing = 0
        f (Just xs) | any (not . isLetter) xs = 0
                    | otherwise = sum . map (ord . toUpper) $ xs
