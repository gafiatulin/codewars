-- Name Array Capping
-- http://www.codewars.com/kata/5356ad2cbb858025d800111d

module Codwars.Kata.NameCapping where

import Data.Char (toUpper, toLower)

capMe :: [String] -> [String]
capMe = map cap
    where cap [] = []
          cap (x:xs) = toUpper x : map toLower xs