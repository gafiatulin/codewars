-- Pascal's Triangle
-- http://www.codewars.com/kata/5226eb40316b56c8d500030f/

module Codewars.Kata.PascalsTriangle where
      
pascalsTriangle :: Int -> [Int]
pascalsTriangle n = concat . take n . iterate f $ [1]
    where f xs = zipWith (+) (0:xs) (xs ++ [0])
