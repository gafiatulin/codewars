-- Hero's root
-- http://www.codewars.com/kata/55efecb8680f47654c000095/

module Codewars.G964.IntSqRoot where

import Data.List (unfoldr)

intRac :: Integer -> Integer -> Integer
intRac n guess = fromIntegral (1 + length (unfoldr f guess))
    where newX x = (x + n `div` x) `div` 2  
          f x = if abs (newX x - x) /= 0 then Just (0, newX x) else Nothing
