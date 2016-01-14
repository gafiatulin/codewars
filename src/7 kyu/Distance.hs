-- Distance from the average
-- http://www.codewars.com/kata/568ff914fc7a40a18500005c/

module Codewars.Kata.Distance where

import Control.Arrow ((&&&))

distancesFromAverage :: [Double] -> [Double]
distancesFromAverage xs = map (avg -) xs
    where avg = uncurry (/) . (sum &&& fromIntegral . length) $ xs
