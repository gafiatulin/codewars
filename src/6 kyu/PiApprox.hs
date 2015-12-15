-- PI approximation
-- http://www.codewars.com/kata/550527b108b86f700000073f/

module Codewars.Kata.PiApprox where

trunc10Dble :: Double -> Double
trunc10Dble d = (fromInteger $ truncate $ d * (10^10)) / (10.0^^10)

iterPi :: Double -> (Integer, Double)
iterPi epsilon = (fromIntegral (1 + length insufficient), trunc10Dble . (approximations !!) . length $ insufficient)
    where approximations = map (4*) . tail . scanl (flip ($)) 0 . zipWith (\f v -> (`f` v)) (cycle [(+), (-)]) . map (1/) $ [1, 3..]
          insufficient = takeWhile ((> epsilon) . abs . (pi -)) approximations
