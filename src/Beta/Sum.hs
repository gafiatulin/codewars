-- Sum Times Tables
-- http://www.codewars.com/kata/551e51155ed5ab41450006e1/

module Codewars.Kata.Sum where

sumTimesTables :: [Integer] -> Integer -> Integer -> Integer
sumTimesTables tbl n m = sum . map (* ((n+m) * (m-n+1) `div` 2)) $ tbl
