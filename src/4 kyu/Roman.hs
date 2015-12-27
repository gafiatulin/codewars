-- Roman Numerals Decoder
-- http://www.codewars.com/kata/51b6249c4612257ac0000005/solutions/haskell

module Roman where

import Data.List (isPrefixOf)
 
solution :: String -> Int
solution "" = 0
solution str = (\(num, xs) -> num + solution xs) 
             . head 
             $ [ (num, drop (length n) str) 
               | (n,num) <- [("M",1000),("CM",900),("D",500),("CD",400),("C",100),("XC",90),("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1)]
               , n `isPrefixOf` str]
