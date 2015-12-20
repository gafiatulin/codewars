-- Valid Parentheses
-- http://www.codewars.com/kata/52774a314c2333f0a7000688/

module Codewars.Parentheses where

validParentheses :: String -> Bool
validParentheses xs = vp xs 0
    where vp [] 0 = True
          vp [] _ = False
          vp ('(':xs) n = vp xs (n+1)
          vp (')':xs) n | n > 0 = vp xs (n-1)
                        | otherwise = False
