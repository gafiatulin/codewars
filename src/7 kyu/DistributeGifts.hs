-- Christmas mission: Distribute gifts #1
-- https://www.codewars.com/kata/584ed874bbf24eb9490001e5

module DistributeGifts.Kata (distributeGifts) where

distributeGifts :: [Int] -> Int
distributeGifts = sum . map ((\(x:xs) -> if null xs then x else product xs) . pFactors)

pFactors n = facts n (2:[3,5..])
facts n (f:fs) | n < f*f = [n]
               | n `mod` f == 0 = f : facts (n `div` f) (f:fs)
               | otherwise = facts n fs
