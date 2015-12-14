-- Grouped by commas
-- http://www.codewars.com/kata/5274e122fc75c0943d000148/

module Codewars.Commas where

import Data.List (unfoldr, intercalate)

groupByCommas :: Int -> String
groupByCommas = reverse . intercalate "," . unfoldr (\rest-> if null rest then Nothing else Just (splitAt 3 rest)) . reverse . show
