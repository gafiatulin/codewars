-- Finding the Closest Maximum Values of a Function to an Upper Limit
-- https://www.codewars.com/kata/56085481f82c1672d000001f

module Codewars.G964.Maxvalfun where

import Data.List (nub, sortBy)

maxValF :: (Int, Int) -> (Int, Int) -> Double -> Int -> [Double]
maxValF range1 range2 hMax k = reverse . take k . nub . sortBy (flip compare) . filter (<= hMax) . map (uncurry f) $ [ (x, y) | x <- [fst range1 .. snd range1], y <- filter (/= x) [fst range2 .. snd range2]]
    where f :: Int -> Int -> Double
          f x y = fromIntegral ((x + y) `div` abs (x - y)) ** fromIntegral (abs (x - y))
