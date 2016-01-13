-- Tube strike options calculator
-- http://www.codewars.com/kata/568ade64cfd7a55d9300003e/

module Codewars.Kata.Tube where

import Codewars.Kata.Tube.Types

calculator :: Double -> Double -> Double -> Decision
calculator distance busDrive busWalk | 60 * (distance / 5) < 10 = Walk
                                     | 60 * (distance / 5) > 120 = Bus
                                     | (distance / 5) <= (busWalk / 5) + (busDrive / 8) = Walk
                                     | otherwise = Bus
