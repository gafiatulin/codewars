-- Sortable Shapes
-- https://www.codewars.com/kata/586669a8442e3fc307000048

module Kata (Shape (..)) where

data Shape = Square {side:: Double} | Rectangle {width:: Double, height:: Double} | Triangle{base :: Double, height :: Double} | Circle {radius :: Double} | CustomShape {area :: Double} deriving (Show)

size :: Shape -> Double
size (Square s) = s^2
size (Rectangle w h) = w*h
size (Triangle b h) = 0.5*b*h
size (Circle r) = pi*r^2
size (CustomShape x) = x

instance Eq Shape where
    x == y = size x == size y

instance Ord Shape where
    compare x y = compare (size x) (size y)
