-- How Much?
-- http://www.codewars.com/kata/55b4d87a3766d9873a0000d4/

module Codewars.G964.Carboat where

howmuch :: Int -> Int -> [[String]]
howmuch m n = map (\m -> [ "M: " ++ show m
                         , "B: " ++ show (m `div` 7)
                         , "C: " ++ show (m `div` 9)]) 
           . filter (\x -> (x `mod` 9) == 1 && (x `mod` 7) == 2) 
           $ [min m n .. max m n]
