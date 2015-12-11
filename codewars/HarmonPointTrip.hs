-- Calculate the Harmonic Conjugated Point of a Triplet of Aligned Points
-- http://www.codewars.com/kata/5600e00e42bcb7b9dc00014e/

module Codewars.G964.HarmonPointTrip where
import Text.Printf (printf)

harmonPointTrip :: Float -> Float -> Float -> String
harmonPointTrip a b c = printf "%.2f" $ (a * (2*b-c) - b*c)/(a+b-2*c)
