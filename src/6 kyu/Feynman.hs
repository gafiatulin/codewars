-- Feynman's square question
-- http://www.codewars.com/kata/551186edce486caa61000f5c/

module Feynman where

countSquares :: Integer -> Integer
countSquares n = n*(n+1)*(2*n+1) `div` 6
