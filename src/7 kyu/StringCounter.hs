-- How many times does it contain?
-- https://www.codewars.com/kata/584466950d3bedb9b300001f

module Kata (stringCounter) where

stringCounter :: String -> Char -> Int
stringCounter s c = length . filter (== c) $ s
