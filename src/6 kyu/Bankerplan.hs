-- Banker's Plan
-- http://www.codewars.com/kata/56445c4755d0e45b8c00010a/

module Codewars.G964.Bankerplan where

fortune :: Int -> Double -> Int -> Int -> Double -> Bool 
fortune f0 p c0 n i = (>=n) . length . takeWhile ((>0) . fst) . iterate f $ (f0, c0)
    where i' = 1 + 0.01*i
          p' = 1 + 0.01*p
          f (f, c) = (fi - c, ci)
              where fi = truncate . (*p') . fromIntegral $ f
                    ci = truncate . (*i') . fromIntegral $ c
