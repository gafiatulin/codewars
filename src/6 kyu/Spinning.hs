-- Stop gninnipS My sdroW!
-- http://www.codewars.com/kata/5264d2b162488dc400000001/

module Codewars.Kata.Spinning where

spinWords :: String -> String
spinWords = unwords . map (\w -> if (<5) . length $ w then w else reverse w) . words
