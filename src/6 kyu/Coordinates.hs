-- Cartesian coordinates from degree angle
-- http://www.codewars.com/kata/555f43d8140a6df1dd00012b/

module Coordinates where

import Numeric (showFFloat)

coordinates :: Double -> Double -> (Double, Double)
coordinates deg r = (r10 $ r * cos φ, r10 $ r * sin φ)
    where φ = deg * pi / 180
          r10 x = read (showFFloat (Just 10) x "") :: Double
