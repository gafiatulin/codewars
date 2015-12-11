-- Area of a Circle
-- http://www.codewars.com/kata/537baa6f8f4b300b5900106c

module Circle where

circleArea :: Double -> Maybe Double
circleArea r | r > 0 = Just( pi * r^2)
             | otherwise = Nothing
