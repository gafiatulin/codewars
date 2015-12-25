-- Help your granny!
-- http://www.codewars.com/kata/5536a85b6ed4ee5a78000035/

module Codewars.Kata.Granny where

import Data.Maybe (mapMaybe)

tour :: [String] -> [(String, String)] -> [(String, Double)] -> Integer
tour friends fTowns distTable = floor . (+ head dofTtoV) . (+ last dofTtoV) . sum . zipWith (\c a -> sqrt(c^2 - a^2)) dofTtoV $ tail dofTtoV
    where dofTtoV = reverse . mapMaybe f $ friends
          f friend = lookup friend fTowns >>= flip lookup distTable
