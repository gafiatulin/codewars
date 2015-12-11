-- Building blocks
-- http://www.codewars.com/kata/55b75fcf67e558d3750000a3

module Codewars.Exercise.Building where

data Block = Block Integer Integer Integer
block :: (Integer, Integer, Integer) -> Block
block (a, b, c) = Block a b c

getWidth (Block a b c) = a
getLength (Block a b c) = b
getHeight (Block a b c) = c
getVolume (Block a b c) = a*b*c
getSurfaceArea (Block a b c) = 2*(a*b + b*c + c*a)
