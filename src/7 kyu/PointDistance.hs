-- Geometry Basics: Distance between points in 3D
-- http://www.codewars.com/kata/58dceee2c9613aacb40000b9

module PointDistance where

import Point

distanceBetweenPoints :: Point -> Point -> Float
distanceBetweenPoints a b = sqrt (((x a - x b) ^ 2) + ((y a - y b) ^ 2) + ((z a - z b) ^ 2))
