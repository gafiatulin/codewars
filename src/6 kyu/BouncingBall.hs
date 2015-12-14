-- Bouncing Balls
-- http://www.codewars.com/kata/5544c7a5cb454edb3c000047/

module Codewars.Kata.BouncingBall where

bouncingBall :: Double -> Double -> Double -> Integer
bouncingBall h b w | h>0 && b>0 && b<1 && w<h = 1 + 2 * floor (logBase b (w/h))
                   | otherwise = -1
