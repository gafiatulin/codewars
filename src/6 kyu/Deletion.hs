-- Delete Nth occurrence of element
-- http://www.codewars.com/kata/554ca54ffa7d91b236000023/

module Codewars.Kata.Deletion where

deleteNth :: [Int] -> Int -> [Int]
deleteNth lst n = foldl (keep n) lst lst
    where keep _ [] _ = []
          keep 0 xs e = filter (/=e) xs
          keep n (x:xs) e = x: keep (if x == e then n-1 else n) xs e
