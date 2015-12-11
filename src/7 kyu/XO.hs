-- Exes and Ohs
-- http://www.codewars.com/kata/55908aad6620c066bc00002a

module Codewars.Kata.XO where

xo :: String -> Bool
xo str = (xx str) == (ox str) where
    xx = length . filter (\x -> elem x "xX")
    ox = length . filter (\x -> elem x "oO")
