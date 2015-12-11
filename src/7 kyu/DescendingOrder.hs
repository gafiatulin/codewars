-- Descending Order
-- http://www.codewars.com/kata/5467e4d82edf8bbf40000155/

module DescendingOrder where

import Data.List (sort)

descendingOrder :: Integer -> Integer
descendingOrder = read . reverse . sort . show
