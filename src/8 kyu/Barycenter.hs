-- Localize The Barycenter of a Triangle
-- http://www.codewars.com/kata/5601c5f6ba804403c7000004

module Codewars.G964.Barycenter where

import Text.Printf (printf)

barTriang :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
barTriang (a, b) (c, d) (e, f) = (read $ printf "%.4f" ((a+c+e)/3 :: Double) , read $ printf "%.4f" ((b+d+f)/3 :: Double))
