-- Rectangle into Squares
-- http://www.codewars.com/kata/55466989aeecab5aac00003e/

module Codewars.Kata.Rectangle where

import Data.List (unfoldr)

squaresInRect :: Integer -> Integer -> Maybe [Integer]
squaresInRect w h | w == h = Nothing
                  | otherwise = Just . unfoldr (\(ww, hh) -> if ww < 1 || hh < 1 then Nothing else Just (min ww hh, (max ww hh - min ww hh, min ww hh))) $ (w, h)
