-- Playing with laziness
-- http://www.codewars.com/kata/5516b80d891547c9b50007fd/

module Laziness where

import Data.List(find)
import Data.Maybe (fromJust)

type Matrix = [[Bool]]

findTrue :: Matrix -> (Int, Int)
findTrue m = fromJust . find (\(i, j) -> ((m !! i) !! j)) $ [(i, l-i) | l<-[0..], i <-[0..l]]
