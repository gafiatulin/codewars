-- Credit Card Mask
-- http://www.codewars.com/kata/5412509bd436bd33920011bc

module Maskify where

maskify :: String -> String
maskify str | length str <= 4 = str
            | otherwise = '#' : maskify (tail str)
