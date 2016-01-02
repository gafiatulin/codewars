-- NIM
-- http://www.codewars.com/kata/54120de842dff35232000195/

module NIM where

import Data.Bits (xor)

chooseMove :: [Int] -> (Int,Int)
chooseMove xs = head . filter (\(i, v) -> v > 0) . zip [0..] . map (\v -> v - (v `xor` x)) $ xs
    where x = foldl1 xor xs
