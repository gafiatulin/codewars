-- Last digits of N^2 == N
-- https://www.codewars.com/kata/584dee06fe9c9aef810001e8

module Green (green) where

import Data.List (isSuffixOf, unfoldr)

green5 = unfoldr f 5
green6 = unfoldr f 6
f x = Just (x, head . filter (g x) . concatMap (h x) $ [1..])
    where g n x = (x > n) && (show x `isSuffixOf` show (x ^ 2))
          h n = map (\p -> read (p ++ show n)) . mapM (const ['0'..'9']) . enumFromTo 1

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

green :: Int -> Integer
green = ((1 : merge green5 green6) !!) . pred
