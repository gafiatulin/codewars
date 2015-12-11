-- Numbers Which Sum of Powers of Its Digits Is The Same Number
-- http://www.codewars.com/kata/560a4962c0cc5c2a16000068/

module Codewars.G964.Powofdigits where

import Data.Char (digitToInt)

eqSumPowDig :: Int -> Int -> [Int]
eqSumPowDig hmax po =  filter (\x -> x == sum (map ((^po) . digitToInt) (show x))) [2 .. hmax]