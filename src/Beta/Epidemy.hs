-- Disease Spread
-- http://www.codewars.com/kata/566543703c72200f0b0000c9/

module Codewars.G964.Epidemy where

epidemic :: Int -> Int -> Double -> Double -> Double -> Double -> Int
epidemic tm n s0 i0 b a = round . fst . maximum . take n . iterate f $ (i0, s0)
    where dt = fromIntegral tm / fromIntegral n
          f (i, s) = (i + dt * (b * s * i - a * i), s - dt * b * s * i)
