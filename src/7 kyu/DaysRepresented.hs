-- How many days are we represented in a foreign country?
-- http://www.codewars.com/kata/58e93b4706db4d24ee000096

module DaysRepresented.JorgeVS.Kata where

import qualified Data.Set as Set

daysRepresented :: [(Int,Int)] -> Int
daysRepresented = Set.size . foldr (\(b, e) s -> Set.union s . Set.fromDistinctAscList $ [b..e]) Set.empty
