-- Collatz
-- http://www.codewars.com/kata/5286b2e162056fd0cb000c20/

module Collatz where 

collatz :: Int -> String
collatz 1 = "1" 
collatz n = show n ++ "->" ++ collatz (if odd n then 3 * n + 1 else n `div` 2)
