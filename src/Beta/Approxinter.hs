-- Floating-point Approximation (II)
-- https://www.codewars.com/kata/581ee0db1bbdd04e010002fd

module Codewars.G964.Approxinter where

interp :: (Double -> Double) -> Double -> Double -> Int -> [Double]
interp f l u n = map ((/100) . fromInteger . floor . (*100) . (/100000) . fromInteger . round . (* 100000) . f) . take n $ [l,l+((u - l) / fromIntegral n)..]
