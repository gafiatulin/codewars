-- Integers: Recreation Two
-- http://www.codewars.com/kata/55e86e212fce2aae75000060/

module Codewars.G964.SqProd2Sum where

import Data.List (sort, nub)

prod2Sum :: Int -> Int -> Int -> Int -> [(Int, Int)]
prod2Sum a b c d = [(a, b) | a <- sqs, b <- dropWhile (<a) sqs, a^2 + b^2 == n] 
    where n = (a*a+b*b)*(c*c+d*d)
          sqs = nub . sort . map abs $ [a*c+b*d, a*c-b*d,a*d+b*c, a*d-b*c]
