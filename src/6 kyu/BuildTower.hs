-- Build Tower
-- https://www.codewars.com/kata/576757b1df89ecf5bd00073b

module Codewars.BuildTower where

buildTower :: Int -> [String]
buildTower h = map (\n -> f n ++ replicate n '*' ++ f n ) . take h $ [1, 3..]
    where l = succ . (*2) . pred $ h
          f n = replicate ((l - n) `div` 2) ' '
