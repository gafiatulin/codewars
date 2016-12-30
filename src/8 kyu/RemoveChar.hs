-- Remove First and Last Character
-- http://www.codewars.com/kata/56bc28ad5bdaeb48760009b0

module Haskell.Codewars.RemoveChar where

removeChar :: String -> String
removeChar = tail . init
