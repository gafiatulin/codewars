-- Triangle number check
-- http://www.codewars.com/kata/557e8a141ca1f4caa70000a6/

module Codewars.Kata.TriangleNumbers where
        
isTriangleNumber :: Integer -> Bool
isTriangleNumber number = n * (n+1) `div` 2 == number
    where n = floor . sqrt . (*2) . fromIntegral $ number
