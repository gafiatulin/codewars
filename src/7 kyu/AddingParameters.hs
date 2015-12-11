-- Gradually Adding Parameters
-- http://www.codewars.com/kata/555b73a81a6285b6ce000047/

module Codewars.Kata.AddingParameters where

add :: Num a => [a] -> a
add  = fst . foldl (\(s, m) v -> (s + m * v, m + 1) ) (0, 1)
