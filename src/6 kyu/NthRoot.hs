-- Nth Root of a Number Redux
-- http://www.codewars.com/kata/552679ea44a9e400b600124f/

module Codewars.Kata.NthRoot where
import Prelude hiding ((**))

root :: Double -> Double -> Double
root x n = exp $ log x / n
