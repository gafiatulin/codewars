-- Divisible Ints
-- http://www.codewars.com/kata/566859a83557837d9700001a/

module Codewars.G964.Divisiblint where

import Data.List (tails, inits)

getCount :: Int -> Int
getCount n = pred . length . filter ((==0) . (n `mod'`) . read) . concatMap (tail . inits) . init . tails . show $ n
    where mod' _ 0 = 1
          mod' m n = mod m n
