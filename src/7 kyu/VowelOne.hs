-- Vowel one
-- https://www.codewars.com/kata/580751a40b5a777a200000a1

module Codewars.Kata.VowelOne where

import Data.Char (toLower)

vowelOne :: String -> String
vowelOne = map ((\c -> if c `elem` "aeioyu" then '1' else '0') . toLower)
