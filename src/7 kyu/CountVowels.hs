-- Count vowels
-- https://www.codewars.com/kata/57a1dd9fcf1fa5d0d100005f

module CountVowels where

countVowels :: String -> Int
countVowels = length . filter (`elem` "aeiou")
