-- Equal Sides Of An Array
-- http://www.codewars.com/kata/5679aa472b8f57fb8c000047/

module Codewars.G964.FindEven where

import Data.List (inits, tails, findIndex)
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))

findEvenIndex :: [Int] -> Int
findEvenIndex = fromMaybe (-1) . findIndex id . uncurry (zipWith (==)) . (map (sum . init) . tail . inits &&& map (sum . tail) . init . tails)
