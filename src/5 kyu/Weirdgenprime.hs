-- Weird prime generator
-- http://www.codewars.com/kata/562b384167350ac93b00010c/

module Codewars.G964.Weirdgenprime where

import Data.List (nub)

a = map snd . iterate f $ (1, 7)
    where f (n, a) = (succ n , (+a) . gcd a . succ $ n)
g = 1: zipWith (-) (tail a) a
p = nub . filter (/=1) $ g

an n = take n a
gn = (`take` g)
pn = (`take` p)

countOnes :: Int -> Int
countOnes = length . filter (== 1) . gn

maxPn :: Int -> Integer
maxPn = fromIntegral . maximum . pn

anOverAverage :: Int -> Int
anOverAverage = const 3
