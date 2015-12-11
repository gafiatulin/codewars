-- Complete The Pattern #3 (Horizontal Image of #2)
-- http://www.codewars.com/kata/557341907fbf439911000022/

module Haskell.Codewars.Pattern where

import Data.List (unfoldr, intercalate)

pattern n = intercalate "\n" . map (\i -> concatMap show . take i . reverse $ [1..n]) $ [1..n]
