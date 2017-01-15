-- String reverse slicing 101
-- https://www.codewars.com/kata/586efc2dcf7be0f217000619

module Kata (reverseSlice) where

import Data.List (tails)

reverseSlice :: String -> [String]
reverseSlice = init . tails . reverse
