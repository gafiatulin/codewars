-- Exponentials as fractions
-- http://www.codewars.com/kata/54f5f22a00ecc4184c000034/

module Codewars.Kata.ExpAsFract where

import Data.Ratio (numerator, denominator, approxRational)
import Control.Arrow ((&&&))

expand :: Double -> Int -> (Integer, Integer)
expand x d = (numerator &&& denominator) . head . dropWhile ((<d) . length . show . numerator) . scanl (+) 0 . zipWith (/) (iterate (*x') 1) $ scanl (*) 1 [1..]
    where x' = approxRational x 1e-6
