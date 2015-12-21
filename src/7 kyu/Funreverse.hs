-- Reversing Fun
-- http://www.codewars.com/kata/566efcfbf521a3cfd2000056/

module Codewars.G964.Funreverse where

reverseFun :: String -> String
reverseFun s = take ((`div` 2) . length $ doubleS) doubleS
    where doubleS = concat . zipWith f (reverse s) $ s
          f c1 c2 = [c1,c2]
