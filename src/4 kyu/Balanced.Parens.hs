-- All Balanced Parentheses
-- http://www.codewars.com/kata/5426d7a2c2c7784365000783/

module Balanced.Parens where

balancedParens = (map bp [0..] !!)
    where bp 0 = [""]
          bp n = ["(" ++ a ++ ")" ++ b | i <- [0 .. n - 1], a <- balancedParens i, b <- balancedParens (n - i - 1)]
