-- Fixed Xor
-- https://www.codewars.com/kata/580f1220df91273ee90001e7

module Codewars.Kata.FixedXor where

import Data.Bits (xor)
import Data.Char(intToDigit, digitToInt)

fixedXor :: String -> String -> String
fixedXor = zipWith (\c1 c2 -> intToDigit (digitToInt c1 `xor` digitToInt c2))
