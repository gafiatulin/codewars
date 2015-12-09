-- Number of Rectangles in a Grid
-- http://www.codewars.com/kata/number-of-rectangles-in-a-grid/train/haskell

module NumberRect where

numberOfRectangles :: Int -> Int -> Int
numberOfRectangles m n = n*m*(n+1)*(m+1) `div` 4
