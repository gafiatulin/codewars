-- Multiply characters
-- http://www.codewars.com/kata/52e9aa89b5acdd26d3000127

module CWSpam where

spam :: Int -> String
spam i = take (3*i) (cycle "hue")
