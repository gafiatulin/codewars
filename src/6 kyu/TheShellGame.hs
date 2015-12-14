-- The Shell Game
-- http://www.codewars.com/kata/546a3fea8a3502302a000cd2/

module TheShellGame where

findTheBall :: Int -> [(Int, Int)] -> Int
findTheBall = foldl swap
    where swap p (p1, p2) | p == p1 = p2
                          | p == p2 = p1
                          | otherwise = p
