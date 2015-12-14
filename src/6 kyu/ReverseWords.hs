-- Reversed Words
-- http://www.codewars.com/kata/51c8991dee245d7ddf00000e/

module ReverseWords where

reverseWords :: String -> String
reverseWords = unwords . reverse . words
