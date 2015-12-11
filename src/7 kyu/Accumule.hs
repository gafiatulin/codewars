-- Mumbling
-- http://www.codewars.com/kata/5667e8f4e3f572a8f2000039/

module Codewars.G964.Accumule where

import Data.Char (toUpper, toLower)
import Data.List (intercalate)

accum :: String -> String
accum =  intercalate "-" . zipWith (\i c -> toUpper c : replicate (i-1) (toLower c)) [1..]
