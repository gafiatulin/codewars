-- Are they the "same"?
-- http://www.codewars.com/kata/550498447451fbbd7600041c/

module Codewars.Kata.Compare where

import Data.List (sort)

comp :: [Integer] -> [Integer] -> Bool
comp as bs | length as == length bs = all id $ zipWith (==) (sort . map (^2) $ as) (sort bs)
           | otherwise = False
