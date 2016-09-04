-- Josephus Survivor
-- http://www.codewars.com/kata/555624b601231dc7a400017a

module Codewars.G964.Josephus where

josephusSurvivor 1 k = 1
josephusSurvivor n k = ((josephusSurvivor (n-1) k + k - 1) `mod` n ) + 1
