-- Fixed-length integer partitions
-- http://www.codewars.com/kata/553291f451ab4fbcdc0001c6/

module IntegerPartitions where

import Control.Monad (replicateM)

indices n d = filter ((==d) . sum) . replicateM n $ [0..d]
