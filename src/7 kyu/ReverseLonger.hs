-- shorter concat [reverse longer]
-- http://www.codewars.com/kata/54557d61126a00423b000a45

module ReverseLonger where

reverseLonger :: String -> String -> String
reverseLonger a b | length a >= length b = b ++ reverse a ++ b
                  | otherwise = a ++ reverse b ++ a
