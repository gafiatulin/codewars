-- Merge in 2048
-- http://www.codewars.com/kata/55e1990978c60e5052000011/

module Haskell.Codewars.Merge2048 where

import Data.List (unfoldr)

merge :: [Int] -> [Int]
merge xs = (ms++) . replicate (length xs - length ms) $ 0
    where ms = unfoldr f . filter (>0) $ xs
          f [] = Nothing
          f [x] = Just (x, [])
          f (x:y:xs) | x == y = Just (x+y, xs)
                     | otherwise = Just (x, y:xs)
