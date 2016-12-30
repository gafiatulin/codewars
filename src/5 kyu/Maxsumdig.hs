-- How Many Numbers? II
-- https://www.codewars.com/kata/55f5efd21ad2b48895000040

module Codewars.G964.Maxsumdig where

import Data.List (genericLength)
import Data.Char (digitToInt)

maxSumDig :: Integer -> Int -> (Int, Integer , Integer)
maxSumDig nmax mxsm = (length list, f list, sum list)
    where h num = let numstring = show num in all (<= mxsm) . map (\x -> sum . map digitToInt . take 4 . drop x $ numstring) $ [0..length numstring-4]
          f = (\(xs, ys) -> if (avg - last xs) < (head ys - avg) then last xs else head ys) . span (<= avg)
          avg = sum list `div` genericLength list
          list = filter h [1000..nmax]
