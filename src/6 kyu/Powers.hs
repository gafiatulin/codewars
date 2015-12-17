-- Compare powers
-- http://www.codewars.com/kata/55b2549a781b5336c0000103/

module Codewars.Exercise.Powers where
import Data.Word
type Power = (Word, Word)

comparePowers :: Power -> Power -> Ordering
comparePowers a b = compare (f a) (f b)
    where f (n, p) = fromIntegral p * log (fromIntegral n)
