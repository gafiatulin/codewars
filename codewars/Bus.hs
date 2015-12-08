-- Number of People in the Bus
-- http://www.codewars.com/kata/5648b12ce68d9daa6b000099

module Codewars.Kata.Bus where

number :: [(Int, Int)] -> Int
number = (\t -> sum (fst t) - sum (snd t) ) . unzip
