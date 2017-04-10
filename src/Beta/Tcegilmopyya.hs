-- Tcegilmopyya
-- https://www.codewars.com/kata/58a26a8e0fd6ed635e000032

module Haskell.Codewars.Tcegilmopyya (sortedShuffle) where

import Data.List (sort, partition)
import Data.Char (toLower)

sortedShuffle :: String -> String
sortedShuffle [] = "Typoglycemia"
sortedShuffle s = unwords . map (concat . f) . words $ s

f :: String -> [String]
f xs | null rest = [g w]
     | otherwise = [g w, [head rest]] ++ f (tail rest)
     where (w, rest) = break (`elem` ".,;:!?()\"-'") xs

g :: String -> String
g [] = []
g [a] = [a]
g [a, b] = [a, b]
g xs | toLower e == toLower b = let (x, y) = partition ((== toLower b) . toLower) m in [b] ++ x ++ sort y ++ [e]
     | otherwise = let (x, y, z) = (\(bs, r) -> let (es, rest) = partition ((== toLower e) . toLower) r in (bs, rest, es)) . partition ((== toLower b) . toLower) $ m in [b] ++ x ++ sort y ++ z ++ [e]
    where m = init . tail $ xs
          b = head xs
          e = last xs
