-- Walter's miraculous FizzBuzz factory
-- http://www.codewars.com/kata/5645d1a4d907bd6009000052/

module Codewars.Kata.FizzBuzz where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)

fizzBuzzFactory :: [(Int, String)] -> Int -> String
fizzBuzzFactory fs x = fromMaybe (show x) . liftM snd . find (\(m, str) -> x `mod` m == 0) . reverse $ fs
