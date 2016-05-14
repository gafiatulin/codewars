-- Ball Upwards
-- http://www.codewars.com/kata/566be96bb3174e155300001b/

module Codewars.G964.Ball where

maxBall :: Int -> Int
maxBall v0 = round $ (10.0 * fromIntegral v0)/(3.6 * 9.81)
