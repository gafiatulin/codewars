-- Braking well
-- http://www.codewars.com/kata/565c0fa6e3a7d39dee000125/

module Codewars.G964.Braking (dist, speed) where

g = 9.81

dist :: Double -> Double -> Double
dist v mu = v' * succ (v' / (2 * mu * g))
    where v' = v / 3.6

speed :: Double -> Double -> Double
speed d mu = 1.8 * (sqrt a * sqrt (a + 4 * d) - a)
    where a = 2 * mu * g
