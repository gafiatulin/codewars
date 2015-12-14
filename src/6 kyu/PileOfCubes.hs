-- Build a pile of Cubes
-- http://www.codewars.com/kata/5592e3bd57b64d00f3000047/

module Codewars.Kata.PileOfCubes where

import Data.Maybe (listToMaybe, fromMaybe)

findNb :: Integer -> Integer
findNb m = fromMaybe (-1) 
         . listToMaybe 
         . filter (\n -> n^2 * (n+1)^2 == 4*m )
         . map (round . (/2)) 
         . zipWith ($) (cycle [((-1)-), ((-1)+)]) 
         . map sqrt 
         . zipWith ($) (cycle [(1-), (1+)]) 
         . replicate 4 . (8 *) . sqrt 
         $ fromIntegral m
