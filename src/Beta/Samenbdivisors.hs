-- Find Numbers with Same Amount of Divisors
-- https://www.codewars.com/kata/55f1614853ddee8bd4000014

module Codewars.G964.Samenbdivisors(countPairsInt) where

countPairsInt :: Int -> Int -> Int
countPairsInt diff nMax = sum . zipWith (\x y -> if x == y then 1 else 0) (take (nMax - diff) list) . drop diff $ list

list = map sigma_0 [1..]
    where sigma_0 n = (+ (if (== n) . (^2) . floor . sqrt . fromIntegral $ n then -1 else 0)) . (*2) . halfNumD $ n
          halfNumD n = length $ 1 : filter ((==0) . rem n) [2 .. floor . sqrt . fromIntegral $ n]
