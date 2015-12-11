-- Width-Height Ratio
-- http://www.codewars.com/kata/55486cb94c9d3251560000ff/

module Codewars.Kata.Ratio where

showRatio :: Int -> Int -> Maybe String
showRatio a b | a <= 0 || b <= 0 = Nothing
              | otherwise = Just( show (a `div` gcd a b) ++":"++ show (b `div` gcd a b))
