-- Cycle a list of values
-- http://www.codewars.com/kata/5456812629ccbf311b000078/

module Cycle where

import Control.Monad (liftM)
import Data.List (elemIndex)

data Direction = L | R deriving (Show, Eq)

cycleList :: (Eq a) => Direction -> [a] -> a -> Maybe a
cycleList d l v = liftM (\i -> (l!!) . (`mod` length l) . (length l +) . (if d == R then succ else pred) $ i) . elemIndex v $ l
