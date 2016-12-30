-- Numbers with this digit inside
-- https://www.codewars.com/kata/57ad85bb7cb1f3ae7c000039

module Kata (numbersWithDigitInside) where

import Data.Char (intToDigit)

numbersWithDigitInside :: Int -> Int -> [Int]
numbersWithDigitInside x d = let a = filter (((intToDigit d) `elem`) . show) $ [1 .. x] in [length a, sum a, if null a then 0 else product a]
