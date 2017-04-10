-- Closest and Smallest
-- https://www.codewars.com/kata/5868b2de442e3fb2bb000119

module Codewars.G964.Closest where

import Data.Tuple (swap)
import Data.Char (digitToInt)
import Control.Arrow ((&&&))
import Data.List (unfoldr, sortBy)
import Data.Maybe (listToMaybe)

closest :: String -> ([Int], [Int])
closest =  maybe ([], []) h . listToMaybe . sortBy g . concat . unfoldr f . map(\(i, (w, v)) -> (i, w, v)) . zip [0..] . map (weight &&& read) . words
    where weight :: String -> Int
          weight = sum . map digitToInt
          f [] = Nothing
          f (x: xs) = Just (map (\s -> (x, s)) xs , xs)
          g ((ai, aw, av), (ai', aw', av')) ((bi, bw, bv), (bi', bw', bv')) = compare (abs (aw' - aw), aw+aw' , ai+ai') (abs (bw' - bw), bw+bw' , bi+bi')
          h ((ai, aw, av), (bi, bw, bv)) | aw < bw = p
                                         | aw > bw = swap p
                                         | otherwise = if ai <= bi then p else swap p
                                         where p = ([aw, ai, av], [bw, bi, bv])
