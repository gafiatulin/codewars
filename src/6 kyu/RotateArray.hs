-- Rotate Array (JS)
-- https://www.codewars.com/kata/54f8b0c7a58bce9db6000dc4

module Codewars.Kata.RotateArray where

rotate :: (Show a, Eq a) => Int -> [a] -> [a]
rotate _ [] = []
{-
rotate shift xs = drop (length xs - shift `mod` length xs) xs ++ take (length xs - shift `mod` length xs) xs
There is a problem with test cases for negative values of shift: expected result is always the initial list, regardless the shift.
-}
rotate shift xs | shift >= 0 = drop (length xs - shift `mod` length xs) xs ++ take (length xs - shift `mod` length xs) xs
                | otherwise = xs
