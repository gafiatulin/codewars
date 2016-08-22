-- John and Ann sign up for Codewars
-- https://www.codewars.com/kata/57591ef494aba64d14000526/


module Codewars.G964.Johnann (ann, john, sumAnn, sumJohn) where

aList :: [Int]
aList = 1 : zipWith (-) [1..] (map (jList !!) aList)
jList :: [Int]
jList = 0 : zipWith (-) [1..] (map (aList !!) jList)

ann :: Int -> [Integer]
ann = map fromIntegral . (`take` aList)
john :: Int -> [Integer]
john = map fromIntegral . (`take` jList)

sumAnn :: Int -> Integer
sumAnn = sum . ann
sumJohn :: Int -> Integer
sumJohn = sum . john
