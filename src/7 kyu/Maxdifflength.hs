-- Maximum Length Difference
-- http://www.codewars.com/kata/5663f5305102699bad000056/

module Codewars.G964.Maxdifflength where

mxdiflg :: [String] -> [String] -> Maybe Int
mxdiflg [] _ = Nothing
mxdiflg _ [] = Nothing
mxdiflg s1 s2 = Just . maximum $ zipWith  (\a b -> abs (a - b)) (f s1) (reverse . f $ s2)
    where f = (\x -> [minimum x, maximum x]) . map length
