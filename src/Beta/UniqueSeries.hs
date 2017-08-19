-- Unique digit sequence
-- https://www.codewars.com/kata/599688d0e2800dda4e0001b0

module Codewars.UniqueSeries (findNum) where

import Data.List (delete, intersect)
import Data.Function (on)

findNum :: Int -> Integer
findNum = (series !!)

series = 0 : f 0 [1..]
    where f n xs = let x = head . filter (null . (intersect `on` show) n) $ xs in x : f x (delete x xs)
