-- Sorting Dictionaries
-- http://www.codewars.com/kata/53da6a7e112bd15cbc000012

module Dictionaries where

import Data.List (sortBy)

sortDict :: Ord v => [(k,v)] -> [(k,v)]
sortDict = sortBy (\(k1, v1) (k2, v2) -> compare v2 v1)
