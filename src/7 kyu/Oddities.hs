-- No oddities here
-- http://www.codewars.com/kata/51fd6bc82bc150b28e0000ce

module Codewars.Oddities where

noOdds :: Integral n => [n] -> [n]
noOdds = filter even
