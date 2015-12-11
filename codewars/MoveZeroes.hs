-- Move Zeros
-- http://www.codewars.com/kata/55c098aa8468f3b9030000f1/

module MoveZeroes where

import Data.List (partition)

move_zeroes :: Bool -> [Int] -> [Int]
move_zeroes isRight = (\(z, xs)-> if isRight then xs ++ z else z++xs) . partition (==0)
