-- Find the K'th Element of a List
-- http://www.codewars.com/kata/5416356b1b28a5e297000bc7/

module Kth where

import Prelude hiding ((!!))

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x 
elementAt (x: xs) n = elementAt xs (n-1)
