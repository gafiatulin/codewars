-- Going to zero or to infinity?
-- http://www.codewars.com/kata/55a29405bc7d2efaff00007c/

module Codewars.Kata.Suite1 where

import Data.Ratio

going :: Integer -> Double
going n = (fromIntegral $ floor (r * 1000000)) / 1000000 
    where r = fromRational $ (sum $ take (fromIntegral n) facs) % (fac n)
          facs :: [Integer]
          facs = scanl1 (*) [2..]
          fac :: Integer -> Integer
          fac n = product [2..n]
