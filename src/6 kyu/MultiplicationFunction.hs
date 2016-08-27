-- Write your own multiplication function
-- https://www.codewars.com/kata/55988922d24a02ccd0000063

module MultiplicationFunction where
import MultiplicationFunction.Preload

multiply :: Number -> Number -> Number
multiply x y | x == zero || y == zero = zero
             | x == one = y
             | y == one = x
             | otherwise = x + multiply x (y - one)
