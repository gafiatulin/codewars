-- Grader
-- http://www.codewars.com/kata/53d16bd82578b1fb5b00128c

module Codewars.Kata.Grade where

grader :: Double -> Char
grader n | n > 1 = 'F'
         | n >= 0.9 = 'A'
         | n >= 0.8 = 'B'
         | n >= 0.7 = 'C'
         | n >= 0.6 = 'D'
         | otherwise = 'F'
