-- Snail
-- http://www.codewars.com/kata/521c2db8ddc89b9b7a0000c1/

module Snail where

import Data.List (transpose)

snail :: [[Int]] -> [Int]
snail [] = []
snail (xs:xss) = xs ++ (snail . reverse . transpose) xss
