-- Which are in?
-- http://www.codewars.com/kata/550554fd08b86f84fe000a58/

module Codewars.Kata.Which where

import Data.List (sort, group, isInfixOf)

inArray :: [String] -> [String] -> [String]
inArray a1 a2 = nub . sort . filter (\s -> any (s `isInfixOf`) a2) $ a1