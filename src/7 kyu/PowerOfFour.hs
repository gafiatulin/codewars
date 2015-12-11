-- Power of 4
-- http://www.codewars.com/kata/544d114f84e41094a9000439/

module PowerOfFour where

isPowerOf4 :: Integral n => n -> Bool
isPowerOf4 n = 4^k == n 
    where k = (round . (4 `logBase`) . fromIntegral) n
