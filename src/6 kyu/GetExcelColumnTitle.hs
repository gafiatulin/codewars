-- Get the Excel column title!
-- http://www.codewars.com/kata/56d082c24f60457198000e77

module Codewars.Kata.GetExcelColumnTitle (getColumnTitle) where

import Control.Monad (liftM2)

alphabet = map (: "") ['A' .. 'Z']

getColumnTitle :: Int -> Maybe String
getColumnTitle n | n <= 0 = Nothing
                 | otherwise = f . pred $ n
                 where f x | x < 26 = Just (alphabet !! x)
                           | otherwise = liftM2 (++) (getColumnTitle d) (f m)
                           where (d, m) = x `divMod` 26
