-- Folding your way to the moon
-- https://www.codewars.com/kata/58f0ba42e89aa6158400000e

module Codewars.Kata.Fold where

thickness = 0.0001

foldTo :: Double -> Maybe Int
foldTo d | d <= 0 = Nothing
         | otherwise = Just . ceiling . logBase 2 $ (d / thickness)
