-- Hyper Sphere
-- http://www.codewars.com/kata/52de9bd621c71b919c000592/

module HyperSphere where

inSphere :: (Ord a, Num a) => [a] -> a -> Bool
inSphere xs r = (<= r^2) . sum . map (^2) $ xs
