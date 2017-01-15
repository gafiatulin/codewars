-- Help Kiyo きよ solve her problems LCM Fun!
-- https://www.codewars.com/kata/5872bb7faa04282110000124

module Kata (kiyoLcm) where

import Data.Either (rights)

kiyoLcm :: [[Either Char Int]] -> Int
kiyoLcm = f . map (sum . filter odd . rights)
    where f xs | null xs = 0
               | otherwise = foldr1 lcm xs
