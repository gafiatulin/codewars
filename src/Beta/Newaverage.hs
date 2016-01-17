-- Looking for a benefactor
-- http://www.codewars.com/kata/569b5cec755dd3534d00000f/

module Codewars.G964.Newaverage where

import Control.Arrow ((&&&))

newAvg :: [Double] -> Double -> Maybe Int
newAvg xs navg = f . uncurry (-) . ((*navg) . succ . snd &&& uncurry (*)) . (uncurry (/) &&& snd) . (sum &&& fromIntegral . length) $ xs
    where f n | n <= 0 = Nothing
              | otherwise = Just . ceiling $ n
