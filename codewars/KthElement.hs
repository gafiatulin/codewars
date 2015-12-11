-- Find the k-th Element of a list safely
-- http://www.codewars.com/kata/54411212cf36259c04000f04/train/haskell

module KthElement where

import Prelude hiding ((!!))

elementAt :: Int -> [a] -> Maybe a
elementAt _ [] = Nothing
elementAt 1 (x:_) = Just x
elementAt n (x:xs) | n > 0 = elementAt (n-1) xs 
                   | otherwise = Nothing
