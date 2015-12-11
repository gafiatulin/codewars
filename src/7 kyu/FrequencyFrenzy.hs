-- Ordinary Ordered Objects
-- http://www.codewars.com/kata/544a247518b8e08af2000251/

module FrequencyFrenzy where

import Data.List (sort, group)
import Control.Arrow ((&&&))

frequency :: Ord a => [a] -> [(a, Int)]
frequency = map (head &&& length) . group . sort
