-- Weight of its Contents
-- http://www.codewars.com/kata/53921994d8f00b93df000bea/

module Codewars.Weights where

contentWeight :: Int -> String -> Int
contentWeight weight str = weight `div` (scale + 1) * if ("smaller" ==) . last . words $ str then 1 else scale
                           where scale = read . head . words $ str
