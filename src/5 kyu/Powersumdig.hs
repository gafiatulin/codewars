-- Numbers that are a power of their sum of digits
-- https://www.codewars.com/kata/55f4e56315a375c1ed000159

module Codewars.G964.Powersumdig(powerSumDigTerm) where

import Data.List(sort)

f x 0 = x
f x n = f (x + r) q
    where (q, r) = n `divMod` 10

ps n = filter ((== n) . sod) . map (n ^) $ [2..20]
    where sod = f 0

pos = sort . concat . filter (/= []) . map ps $ [2..120]

powerSumDigTerm :: Int -> Integer
powerSumDigTerm = (pos !!) . pred
