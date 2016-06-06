-- Mythical Heads and Tails
-- http://www.codewars.com/kata/5751aa92f2dac7695d000fb0

module Codewars.Kata.Beats where

beasts :: Integer -> Integer -> Maybe (Integer, Integer)
beasts heads tails | (&& (h >= 0)) . (&& (o >= 0)) . (== 0) . snd $ t = Just (o, h)
                   | otherwise = Nothing
                   where t = (heads - 2 * tails) `divMod` 3
                         h = fst t
                         o = tails - h
