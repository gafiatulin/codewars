-- Does my number look big in this?
-- http://www.codewars.com/kata/5287e858c6b5a9678200083c/

module Narcissistic where

import Data.Char (digitToInt)
narcissistic :: Integral n => n -> Bool
narcissistic n = (== fromIntegral n) .  sum . map ((^(length . show . fromIntegral $ n)) . digitToInt) . show . fromIntegral $ n
