-- Vowel Count
-- http://www.codewars.com/kata/54ff3102c1bad923760001f3

module Codewars.Kata.Vowel where

getCount :: String -> Int
getCount = length . filter (`elem` "aeiou")
