-- Invert values
-- https://www.codewars.com/kata/5899dc03bc95b1bf1b0000ad

module Kata (invert) where

invert :: [Integer] -> [Integer]
invert = map negate
