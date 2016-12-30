-- Find min and max
-- https://www.codewars.com/kata/57a1ae8c7cb1f31e4e000130

module MinMax where

import Control.Arrow ((&&&))

getMinMax :: [Int] -> (Int, Int)
getMinMax = minimum &&& maximum
