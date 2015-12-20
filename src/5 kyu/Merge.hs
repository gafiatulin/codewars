-- MergeSort "merge" function
-- http://www.codewars.com/kata/52336a4436e0b095d8000093/

module Merge where


merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
