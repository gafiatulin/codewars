-- Return the Missing Element
-- http://www.codewars.com/kata/5299413901337c637e000004

module MissingElement where

import Data.List((\\))

getMissingElement :: [Int] -> Int
getMissingElement = head . (\\) [0..9]
