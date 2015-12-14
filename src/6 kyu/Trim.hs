-- Trim a String
-- http://www.codewars.com/kata/526e8de0512511429e000006/

module Trim where

import Data.Char (isSpace)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
