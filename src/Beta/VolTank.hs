-- Tank Truck
-- http://www.codewars.com/kata/55f3da49e83ca1ddae0000ad/

module Codewars.G964.VolTank where

tankvol :: Int -> Int -> Int -> Int
tankvol h d = floor . (/(pi*r*r)) . (*s) . fromIntegral
    where h' = fromIntegral h
          r = fromIntegral d / 2
          s = (r^2 * acos ((r-h')/r)) - (r - h') * sqrt (2 * r * h' - h' * h')
