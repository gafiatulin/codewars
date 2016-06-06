-- How Many Numbers?
-- http://www.codewars.com/kata/55d8aa568dec9fb9e200004a

module Codewars.G964.Howmany where

import Data.Char (digitToInt)
import Data.List (sort, group)

selNumber :: Int -> Int -> Int
selNumber n d = length . filter (((== xs) . map head . group . sort $ xs) && (all (<= d) . zipWith (-) (tail xs) $ xs)) . map (map digitToInt . show) $ [10 .. n]
