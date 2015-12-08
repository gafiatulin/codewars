-- Last
-- http://www.codewars.com/kata/541629460b198da04e000bb9

module Last where

import Prelude hiding (last)

last :: [a] -> a
last [x] = x
last (x:xs) = last xs
