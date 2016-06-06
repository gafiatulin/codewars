-- Highest number with two prime factors
-- http://www.codewars.com/kata/55f347cfb44b879e1e00000d

module Codewars.G964.Highest2factors where

import Data.List (maximumBy)
import Data.Ord (comparing)

highestBiPrimefac :: Integer -> Integer -> Integer -> (Integer, Int, Int)
highestBiPrimefac p1 p2 n = maximumBy (comparing (\(x, _, _) -> x)) [(p1 ^ e1 * p2 ^ e2, e1, e2) | e1 <- f p1 , e2 <- f p2, p1 ^ e1 * p2 ^ e2 <= n]
    where f x = [1 .. floor (logBase (fromIntegral x) (fromIntegral n))]
