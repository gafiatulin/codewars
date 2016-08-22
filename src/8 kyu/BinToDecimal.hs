-- Bin to Decimal
-- https://www.codewars.com/kata/57a5c31ce298a7e6b7000334

module BinToDecimal where
import Data.Char (digitToInt)

binToDec :: String -> Int
binToDec = sum . zipWith (\p n -> n*2^p) [0..] .  map digitToInt . reverse
