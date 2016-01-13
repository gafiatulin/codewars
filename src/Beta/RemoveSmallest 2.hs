-- Another one down—the Survival of the Fittest!
-- http://www.codewars.com/kata/563ce9b8b91d25a5750000b6/

module Codewars.Kata.RemoveSmallest where

import Data.List (delete)

removeSmallest :: Int -> [Int] -> [Int]
removeSmallest n | n <= 0 = id
                 | otherwise = (!!n) . iterate f
                 where f xs = delete (minimum xs) xs
