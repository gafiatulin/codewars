-- Fill in the gaps in my timesheet.
-- http://www.codewars.com/kata/564871e795df155582000013/

module Codewars.Kata.FillTheGaps where

import Data.List (group)
import Data.Maybe (isNothing)
type Task = Maybe Int

fillGaps :: [Task] -> [Task]
fillGaps = concat . f . group
    where f (x:y:z:xs) | (isNothing . head $ y) && head x == head z = x : map (const . head $ x) y : f(z:xs)
                       | otherwise = x:f(y:z:xs)
          f x = x
