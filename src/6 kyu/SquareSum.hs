-- Square(n) Sum
-- http://www.codewars.com/kata/515e271a311df0350d00000f/

module SquareSum where

squareSum :: [Integer] -> Integer
squareSum = sum . map (^2)
