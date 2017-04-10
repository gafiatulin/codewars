-- longest_palindrome
-- https://www.codewars.com/kata/54bb6f887e5a80180900046b

module Codewars.Kata.LongestPalindrome (longestPalindrome) where

import Data.Maybe (listToMaybe)
import Data.Ord (Down(..))
import Data.List (sortOn)
import Data.Array (assocs, listArray, (!))

longestPalindrome :: Eq a => [a] -> Int
longestPalindrome = maybe 0 f . listToMaybe . sortOn (\(a, b, x) -> Down (x, b-a)) . manacher
    where f (p1, p2, l) | p1 == p2 = succ . (*2) . pred $ l
                        | otherwise = 2 * l


manacher :: Eq a => [a] -> [(Int, Int, Int)]
manacher str = [(div i 2, div (i + 1) 2, e) | (i, e) <- assocs b]
    where n = length str
          a = listArray (1, n) str
          pal l r | l < 1 || r > n = 0
                  | a!l /= a!r = 0
                  | otherwise = succ $ pal (pred l) (succ r)
          b = listArray (2, 2*n) $ f 2 (0, 0)
          f k (e, c) = if l > n then [] else t: f (k + 1) (e', c')
              where l = div k 2
                    r = k - l
                    s = if k <= e then min (b!(2*c-k)) $ 1 + div (e-k) 2 else 0
                    t = s + pal (l - s) (r + s)
                    (e', c') = max (e, c) (2 * (r + t - 1), k)
