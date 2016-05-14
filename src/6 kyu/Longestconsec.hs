-- Consecutive strings
-- http://www.codewars.com/kata/56a5d994ac971f1ac500003e/

module Codewars.G964.Longestconsec where

import Data.List (unfoldr, maximumBy)
import Control.Arrow ((&&&))
import Data.Function (on)

longestConsec :: [String] -> Int -> String
longestConsec strarr k | n == 0 || k > n || k <= 0 = ""
                       | otherwise = snd . maximumBy (compare `on` fst) . map (length &&& id) . unfoldr f $ (strarr, n)
                       where n = length strarr 
                             f (strs, n) = let (ss, sf) = splitAt (n - k) strs in if length sf < k then Nothing else Just (concat sf, (ss ++ init sf, pred n))
