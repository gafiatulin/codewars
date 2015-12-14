-- Highest Rank Number in an Array
-- http://www.codewars.com/kata/5420fc9bb5b2c7fd57000004/

module Term where
import Data.List (sort, group , maximumBy)

highestRank :: Ord c => [c] -> c
highestRank = head . maximumBy cmp . group . sort
    where cmp a b = case compare (length a) (length b) of 
              EQ -> compare a b
              x -> x
