-- Scaling Squared Strings
-- http://www.codewars.com/kata/56ed20a2c4e5d69155000301

module Codewars.G964.Scalesqstrings where

scale :: String -> Int -> Int -> String
scale [] _ _ = ""
scale s k n = init . unlines . concatMap (replicate n . concatMap (replicate k)) . lines $ s