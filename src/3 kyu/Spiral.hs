-- Make a spiral
-- http://www.codewars.com/kata/534e01fbbb17187c7e0000c6/

module Spiral where

spiralize :: Int -> [[Int]]
spiralize n | n == 5 = [[1,1,1,1,1],[0,0,0,0,1],[1,1,1,0,1],[1,0,0,0,1],[1,1,1,1,1]]
            | n == 6 = [[1,1,1,1,1,1],[0,0,0,0,0,1],[1,1,1,1,0,1],[1,0,0,1,0,1],[1,0,0,0,0,1],[1,1,1,1,1,1]]
            | otherwise = fl:sl:rest
            where fl = replicate n 1
                  sl = replicate (n-1) 0 ++ [1]
                  rest = zipWith (++) (map reverse . reverse .spiralize $ (n-2)) (replicate (n-3) [0, 1] ++ [[1, 1]])
