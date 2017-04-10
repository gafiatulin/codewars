-- Remove String Spaces
-- https://www.codewars.com/kata/57eae20f5500ad98e50002c5

module Kata (noSpace) where

import Data.Char (isSpace)

noSpace :: String -> String
noSpace = filter (not . isSpace)
