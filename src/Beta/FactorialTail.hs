-- Factorial tail
-- http://www.codewars.com/kata/55c4eb777e07c13528000021/

module Codewars.Kata.FactorialTail where

import Data.List (group)
import Control.Arrow ((&&&))

zeroes :: Integral a => a -> a -> a
zeroes base n = minimum . map (f . (head &&& length)) . group . pd $ base
    where f (a, b) = (`div` fromIntegral b) . sum . map (n `div`) $ [a^i | i <- [1.. floor (logBase (fromIntegral a) (fromIntegral n))]]
          pd x | x == 1 = []
               | null d = [x]
               | otherwise = head d : pd (x `div` head d)
               where d = filter ((==0) . (x `mod`)) [2.. floor . sqrt . fromIntegral $ x]
