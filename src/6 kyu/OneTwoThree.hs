-- Begin your day with a challenge, but an easy one.
-- https://www.codewars.com/kata/5873b2010565844b9100026d

module Kata (oneTwoThree) where

oneTwoThree :: Integer -> [Integer]
oneTwoThree 0 = [0, 0]
oneTwoThree n = [read . (++ f r) . replicate (fromInteger q) $ '9', read . replicate (fromInteger n) $ '1']
    where f 0 = ""
          f n = show n
          (q, r) = n `divMod` 9
