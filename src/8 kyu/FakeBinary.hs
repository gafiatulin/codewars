-- Fake Binary
-- https://www.codewars.com/kata/57eae65a4321032ce000002d

module Codewars.Kata.FakeBinary where

fakeBin :: String -> String
fakeBin = map (\c -> if c < '5' then '0' else '1' )
