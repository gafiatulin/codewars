-- Sum Arrays
-- http://www.codewars.com/kata/53dc54212259ed3d4f00071c

module Sum where

import Prelude hiding (sum)

sum :: Num a => [a] -> a
sum = foldl (+) 0
