-- Are the brackets balanced?
-- http://www.codewars.com/kata/554e6241d665537588000079/

module Codewars.Exercises.Balanced where

isBalanced :: String -> Bool
isBalanced = vp 0 . filter (`elem` "()")
    where vp 0 [] = True
          vp _ [] = False
          vp n ('(':xs) = vp (n+1) xs 
          vp n (')':xs) | n > 0 = vp (n-1) xs 
                        | otherwise = False
