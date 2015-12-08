-- Functions of Integers on Cartesian Plane
-- http://www.codewars.com/kata/559e3224324a2b6e66000046

module Codewars.Kata.Funcij where

sumin :: Integer -> Integer
sumin n = (n * (n + 1) * (2*n+1)) `div` 6 
sumax :: Integer -> Integer
sumax n = (n * (n + 1) * (4*n-1)) `div` 6 
sumsum :: Integer -> Integer
sumsum n = sumax n + sumin n
