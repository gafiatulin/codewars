-- Caesar Cipher Sorting
-- http://www.codewars.com/kata/5483b69b48cf540cfc000119/

module CaesarSort where

import Data.Char (ord)
import Data.List (partition, unfoldr)
import Control.Arrow (first)

caesarSort :: [String] -> [[String]]
caesarSort = unfoldr f
    where f [] = Nothing
          f (x:xs) = Just . first (x :) . partition (g x) $ xs
          g a b | length a /= length b = False
                | otherwise = same . zipWith h a $ b
          h c1 c2 = ((ord c1 - ord 'a') - (ord c2 - ord 'a')) `mod` 26
          same [] = True
          same (x:xs) = all (==x) xs
