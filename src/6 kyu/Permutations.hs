-- Number of permutations without repetitions
-- http://www.codewars.com/kata/571bff6082661c8a11000823

module Codewars.Kirilloid.Permutations where

import Data.List (genericLength, sort, group)

perms :: Ord a => [a] -> Integer
perms xs = ((factorial . genericLength $ xs) `div`) . product . map (factorial . genericLength) . group . sort $ xs
    where factorial x = product [1.. x]
