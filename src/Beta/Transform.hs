-- transform an array into a string
-- https://www.codewars.com/kata/59a602dc57019008d900004e

module Transform.Kata (transform) where

transform :: (Show a) => [a] -> String
transform = concatMap show
