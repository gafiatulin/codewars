-- Hex to Decimal
-- https://www.codewars.com/kata/57a4d500e298a7952100035d

module HexToDecimal where
import Numeric(readHex)

hexToDec :: String -> Int
hexToDec = fst . head . readHex
