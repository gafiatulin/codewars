-- repeatIt
-- http://www.codewars.com/kata/557af9418895e44de7000053

module Codewars.Exercises.RepeatIt where

repeatIt :: String -> Int -> String
repeatIt str n = take (n * length str) (cycle str)
