-- Getting along with Integer Partitions
-- http://www.codewars.com/kata/55cf3b567fc0e02b0b00000b/

module Codewars.G964.Partition (part) where

import Data.List (group, sort)
import Text.Printf (printf)
import Control.Arrow ((&&&))

part :: Int -> String
part n | n == 1 = "Range: 0 Average: 1.00 Median: 1.00"
       | otherwise = printf "Range: %d Average: %.2f Median: %.2f" (last ps - head ps) a m
        where (a, m) = g ps
              ps = map head . group . sort . map product . f n $ n

g :: [Int] -> (Double, Double)
g = uncurry (/) . (fromIntegral . sum &&& fromIntegral . length) &&& median

f :: Int -> Int -> [[Int]]
f _ 0 = [[]]
f h n = [ a:as | a<-[1..min n h], as <- f a (n-a) ]

median :: [Int] -> Double
median xs | odd len = fromIntegral (xs !! mid)
        | otherwise = (/2) . fromIntegral . sum . take 2 . drop (mid-1) $ xs
        where len = length xs
              mid = len `div` 2
