-- Wave Sorting
-- https://www.codewars.com/kata/596f28fd9be8ebe6ec0000c1

module Kata.WaveSort (waveSort) where

import Data.List (sort)

waveSort :: (Eq x,Ord x) => [x] -> [x]
waveSort = wave . sort
    where  wave [] = []
           wave [x] = [x]
           wave (x:y:xs) = y:x:wave xs
