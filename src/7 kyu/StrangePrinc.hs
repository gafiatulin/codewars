-- Strange principal
-- http://www.codewars.com/kata/55fc061cc4f485a39900001f/

module Codewars.G964.StrangePrinc where

numOpenLockers :: Int -> Int
numOpenLockers n = floor . sqrt . fromIntegral
