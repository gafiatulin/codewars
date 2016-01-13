-- Sum consecutives
-- http://www.codewars.com/kata/55eeddff3f64c954c2000059/

module Codewars.G964.Sumconsec where

import Data.List (group)

sumConsecutives :: [Int] -> [Int]
sumConsecutives = map sum . group
