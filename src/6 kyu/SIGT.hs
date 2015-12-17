-- String Integer Greater-than
-- http://www.codewars.com/kata/54d3eae3525c153b21000e3b

module Codewars.Kata.SIGT where
import Prelude hiding (read, reads, readsPrec, Integer, fromIntegral, fromInteger, toInteger)

stringIntGreaterThan :: String -> String -> Bool
stringIntGreaterThan s1 s2 = case (head s1, head s2) of
    ('-', '-') -> g (tail s2) (tail s1)
    ('-',  _ ) -> False
    ( _ , '-') -> True
    ( _ ,  _ ) -> g s1 s2
    where g a b = (length a > length b) || ((length a == length b) && (a > b))
