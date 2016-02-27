-- Next Featured Number Higher than a Given Value
-- http://www.codewars.com/kata/56abc5e63c91630882000057/

module Codewars.Kata.NumberMagic where

import Control.Arrow ((&&&))
import Data.Maybe (listToMaybe)
import Data.List (find, nub)

next = (>>= g) . f
    where f x = find ((==0) . (`mod` 3)) [x+1, x+2, x+3]
          g x = listToMaybe . filter (uncurry (==) . (id &&& nub) . show) . filter odd $ [x, x+3 .. 9999999999]
