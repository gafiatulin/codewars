-- Guess the array
-- https://www.codewars.com/kata/59392ff00203d9686a0000c6

module GuessTheArray where

guess :: Integral n => (Int -> Int -> n) -> Int -> [n]
guess f i | i > 3 =  guess f (i - 1) ++ [g (i - 3) - f (i - 3) (i - 2)]
          | otherwise = [g 0 - f 1 2, g 0 - f 0 2, g 0 - f 0 1]
          where g n = (f n (n + 1) + f n (n + 2) + f (n + 1) (n + 2)) `div` 2
