-- Growth of a Population
-- http://www.codewars.com/kata/563b662a59afc2b5120000c6/train/haskell

module Codewars.G964.Arge where

nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p | p <= p0 = 0
                        | otherwise = 1 + nbYear px percent aug p
                        where px = p0 + aug + round (0.01 * percent * fromIntegral p0)
