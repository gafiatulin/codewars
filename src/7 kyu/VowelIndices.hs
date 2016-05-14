-- Find the vowels
-- http://www.codewars.com/kata/5680781b6b7c2be860000036/

module VowelIndices where

import Data.List (findIndices)

vowelIndices :: String -> [Integer]
vowelIndices = map (fromIntegral . succ) . findIndices (`elem` "aeiouyAEIOUY")
