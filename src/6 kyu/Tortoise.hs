-- Tortoise racing
-- http://www.codewars.com/kata/55e2adece53b4cdcb900006c/

module Codewars.G964.Tortoise where

race :: Int -> Int -> Int -> Maybe (Int, Int, Int)
race v1 v2 g | v1 >= v2 = Nothing
             | otherwise = Just (h, m, s)
             where t = 3600 * fromIntegral g / fromIntegral (v2-v1)
                   h = floor (t/3600)
                   m = floor (t/60) `mod` 60
                   s = floor t `mod` 60
