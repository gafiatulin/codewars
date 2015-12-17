-- Playing on a chessboard
-- http://www.codewars.com/kata/55ab4f980f2d576c070000f4/

module Codewars.G964.Suite2 where

game :: Integer -> Either Integer (Integer, Integer)
game n | even $ n^2 = Left (n^2 `div` 2)
       | otherwise = Right (n^2 , 2)
