-- Simple decrypt algo
-- https://www.codewars.com/kata/58693136b98de0e4910001ab

module Kata (decrypt) where

import Data.Char (isAsciiLower, intToDigit)
import qualified Data.Map.Strict as Map

decrypt :: String -> String
decrypt = map (intToDigit . snd) . Map.toAscList . foldl f (Map.fromList . zip ['a' .. 'z'] . repeat $ 0)
  where f m c | isAsciiLower c = Map.insertWith (+) c 1 m
              | otherwise = m
