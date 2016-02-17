-- Reverse or rotate?
-- http://www.codewars.com/kata/56b5afb4ed1f6d5fb0000991/

module Codewars.G964.Revrot where

import Data.Char (digitToInt)
import Data.List.Split (chunksOf)

revRot :: String -> Int -> String
revRot [] _ = ""
revRot s sz | sz <= 0 || sz > l = ""
            | otherwise = concatMap f . take (l `div` sz) . chunksOf sz $ s
            where l = length s
                  f xs | even . sum . map ((^3) . digitToInt) $ xs = reverse xs
                       | otherwise = tail xs ++ [head xs]
