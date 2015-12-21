-- Diophantine Equation
-- http://www.codewars.com/kata/554f76dca89983cc400000bb/

module Codewars.Kata.Dioph where

solequa :: Integer -> [(Integer, Integer)]
solequa n = map (\(b, a) -> (a + (b-a) `div` 2, (b-a) `div` 4)) . filter ((==0) . (`mod` 4) . uncurry (-)) . map (\d -> (n `div` d, d)) $ k
    where k = filter (\d -> n `mod` d == 0) [1.. floor . sqrt . fromIntegral $ n]
