-- Cartesian neighbors
-- http://www.codewars.com/kata/58989a079c70093f3e00008d

module Cartesian.Neighbor.JorgeVS where

cartesianNeighbor :: Int -> Int -> [(Int,Int)]
cartesianNeighbor x y = [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], (i, j) /= (x, y)]
