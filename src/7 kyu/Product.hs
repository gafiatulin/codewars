-- Product of the main diagonal of a square matrix.
-- http://www.codewars.com/kata/551204b7509063d9ba000b45/

module Codewars.Kata.Product where

mainDiagonalProduct :: Num a => [[a]] -> a
mainDiagonalProduct = snd . foldl (\(i, p) row -> (i+1, p * (row !! i))) (0, 1)
