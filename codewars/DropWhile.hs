-- The dropWhile Function
-- http://www.codewars.com/kata/54f9c37106098647f400080a/

module Codewars.Exercise.DropWhile where
import Prelude hiding (dropWhile, span, break)

dropWhile :: [a] -> (a -> Bool) -> [a]
dropWhile [] _ = []
dropWhile (x:xs) p | p x = dropWhile xs p
                   | otherwise = x : xs
