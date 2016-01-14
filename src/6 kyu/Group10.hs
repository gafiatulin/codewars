-- Group in 10s
-- http://www.codewars.com/kata/5694d22eb15d78fe8d00003a/

module Codewars.Kata.Group10 where

import Data.List (sort, groupBy)
import Data.Function (on)
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

groupIn10s :: [Int] -> [[Int]]
groupIn10s [] = []
groupIn10s xs = map (fromMaybe [] . (`lookup` d)) [0.. (`div` 10) . maximum $ xs]
    where d = map ((`div` 10) . head &&& id) . groupBy ((==) `on` (`div` 10)) . sort $ xs
