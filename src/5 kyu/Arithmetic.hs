-- Find the missing term in an Arithmetic Progression
-- http://www.codewars.com/kata/52de553ebb55d1fca3000371/

module Codewars.Kata.Arithmetic where

findMissing :: Integral n => [n] -> n
findMissing (a:b:c:xs) | b-a == c-b = fst . head . filter (uncurry (/=)) . zip [a,b..] $ (a:b:c:xs)
                       | abs (b-a) < abs (c-b) = 2*b-a
                       | abs (b-a) > abs (c-b) = a + c - b