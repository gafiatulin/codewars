-- Sum of squares less than some number
-- https://www.codewars.com/kata/57b71a89b69bfc92c7000170

module NumberOfSquares where

getNumberOfSquares :: Int -> Int
getNumberOfSquares n = pred . head . dropWhile (\k-> k*(k+1)*(2*k+1) < 6 * n) $ [0..]
