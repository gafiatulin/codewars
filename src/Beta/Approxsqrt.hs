-- Floating-point Approximation (I)
-- https://www.codewars.com/kata/58184387d14fc32f2b0012b2

module Codewars.G964.Approxsqrt (f) where

f :: Double -> Double
f x = x / (1.0 + sqrt(1.0 + x))
