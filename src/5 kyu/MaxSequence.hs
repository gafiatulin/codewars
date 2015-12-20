-- Maximum subarray sum
-- http://www.codewars.com/kata/54521e9ec8e60bc4de000d6c/

module MaxSequence where

import Data.List (inits)

maxSequence :: [Int] -> Int
maxSequence [] = 0
maxSequence (x:xs) = if m < 0 then 0 else m
    where m = max (maximum . map ((+x) . sum) . inits $ xs) (maxSequence xs)
