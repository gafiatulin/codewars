-- Stringy Strings
-- http://www.codewars.com/kata/563b74ddd19a3ad462000054

module Codewars.Stringifier where

stringy :: Int -> String
stringy n = take n (cycle "10")
