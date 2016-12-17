-- Help Bob count letters and digits.
-- https://www.codewars.com/kata/5738f5ea9545204cec000155

module Kata where

import Data.Char (isAlphaNum)

countLettersAndDigits :: String -> Int
countLettersAndDigits = length . filter isAlphaNum
