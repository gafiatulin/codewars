-- Hutton's Razor
-- http://www.codewars.com/kata/543833d86f032f0942000264/

module Razor where

data Razor = Lit Int | Add Razor Razor
  
interpret :: Razor -> Int
interpret (Lit a) = a
interpret (Add a b) = interpret a + interpret b

pretty :: Razor -> String
pretty (Lit a) = show a
pretty (Add a b) = "(" ++ pretty a ++ "+" ++ pretty b ++ ")" 
