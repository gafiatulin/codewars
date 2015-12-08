-- esreveR
-- http://www.codewars.com/kata/5413759479ba273f8100003d

module Reverse where

import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse = foldr (\a b -> b ++ [a]) []
