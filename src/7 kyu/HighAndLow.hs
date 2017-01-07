-- Highest and Lowest
-- https://www.codewars.com/kata/554b4ac871d6813a03000035

module Kata (highAndLow) where

highAndLow :: String -> String
highAndLow s = let ns = map (\ss -> read ss :: Int) . words $ s in unwords . map show $ [maximum ns, minimum ns]
