-- Figurate Numbers #1 - Pentagonal Number
-- http://www.codewars.com/kata/55ab9eee6badbdaf72000075/

module Codewars.G964.Penta (pNum, gpNum, spNum) where

import Control.Arrow ((&&&))

f :: (Int -> [Int]) -> Int -> Bool
f g = uncurry (&&) . (uncurry (==) . (floor . ff &&& ceiling . ff) &&& any ((== 5) . (`mod` 6)) . g . floor . ff)
ff :: Int -> Float
ff = sqrt . succ . (*24) . fromIntegral

pNum :: Int -> Bool
pNum = f (: [])
gpNum :: Int -> Bool
gpNum = f (\a -> [a, negate a])
spNum :: Int -> Bool
spNum = uncurry (&&) . (uncurry (==) . (floor . sqrt . fromIntegral &&& ceiling . sqrt . fromIntegral) &&& pNum)
