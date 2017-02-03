-- No yelling!
-- https://www.codewars.com/kata/587a75dbcaf9670c32000292

module Kata (filterWords) where

import Data.Char(toLower, toUpper)

filterWords :: String -> String
filterWords = f . unwords . words
  where f [] = []
        f (x:xs) = toUpper x : map toLower xs
