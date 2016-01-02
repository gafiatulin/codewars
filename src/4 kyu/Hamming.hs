-- Hamming Numbers
-- http://www.codewars.com/kata/526d84b98f428f14a60008da/

module Hamming where

hamming  :: Int -> Int
hamming n = (!!(n-1)) . hns $ n
    where hns 1 = [1]
          hns n = (\(a, b : bs) -> a ++ b: merge bs [2*b, 3*b, 5*b]) . splitAt (n-2) . hns $ (n-1)
          merge xs [] = xs
          merge [] ys = ys
          merge (x:xs) (y:ys) | x < y = x:merge xs (y:ys)
                              | x == y = x: merge xs ys
                              | otherwise = y:merge (x:xs) ys
