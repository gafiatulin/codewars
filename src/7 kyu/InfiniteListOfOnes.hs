-- Infinite list of 1's
-- https://www.codewars.com/kata/57d0695142e44e65e8001346

module InfiniteListOfOnes where
import Prelude hiding (take,repeat,cycle,replicate)

getInfiniteListOfOnes :: [Int]
getInfiniteListOfOnes = iterate id 1
