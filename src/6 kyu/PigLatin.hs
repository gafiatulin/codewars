-- Single Word Pig Latin
-- http://www.codewars.com/kata/558878ab7591c911a4000007/

module Codewars.PigLatin where

import Data.Char (isLetter, toLower)

pigLatin :: String -> Maybe String
pigLatin xs | any (not . isLetter) xs' = Nothing
            | all (not . isVowel) xs' = Just (xs' ++ "ay")
            | isVowel . head $ xs' = Just (xs' ++ "way")
            | otherwise = Just (v ++ c ++ "ay")
            where xs' = map toLower xs
                  (c, v) = break isVowel xs'
                  isVowel c = c `elem` "aeiou"
