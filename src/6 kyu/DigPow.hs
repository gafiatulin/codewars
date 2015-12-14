-- Playing with digits
-- http://www.codewars.com/kata/5552101f47fc5178b1000050/

module Codewars.Kata.DigPow where

import Data.Char (digitToInt)

digpow :: Integer -> Integer -> Integer
digpow n p = fromIntegral (if k `mod` n' == 0 then k `div` n' else -1)
    where n' = fromIntegral n
          k = sum . zipWith (^) (map digitToInt . show $ n') $ [p..]
