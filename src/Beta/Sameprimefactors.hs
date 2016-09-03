-- Numbers and its Reversal Having Same Prime Factors.
-- https://www.codewars.com/kata/55ea170313b76622b3000014

module Codewars.G964.Sameprimefactors(sameFactRev) where

import Data.List(nub)

sameFactRev :: Int -> [Int]
sameFactRev nmax = takeWhile (< nmax) numbers

numbers = 1089 : filter f [1090..]
    where f x = let (r, pf) = (rev x, pfactors x) in x /= r && r >= 1089 && pf /= [] && pf == (pfactors r)

pfactors = nub . factor

factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
           in (prime :) $ factor $ div n prime

rev :: Int -> Int
rev = f 0
  where f n 0 = n
        f n x = let (d,m) = x `divMod` 10 in f (n*10+m) d
