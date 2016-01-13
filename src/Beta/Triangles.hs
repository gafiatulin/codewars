-- Is this a triangle?
-- http://www.codewars.com/kata/56606694ec01347ce800001b/

module Codewars.Triangles where

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = and [a < b+c, b < c+a, c < a+b]
