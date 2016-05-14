-- How Green Is My Valley?
-- http://www.codewars.com/kata/56e3cd1d93c3d940e50006a4

module Codewars.G964.Makevalley where

import Data.List(sortBy)

makeValley :: [Int] -> [Int]
makeValley = f . sortBy (flip compare)
  where f [] = []
        f [x] = [x]
        f (x:y:xs) = x : f xs ++ [y]