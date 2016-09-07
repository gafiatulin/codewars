-- Factorial
-- https://www.codewars.com/kata/57a049e253ba33ac5e000212

module Factorial where

factorial 0 = 1
factorial 1 = 1
factorial n = product [2 .. n]
