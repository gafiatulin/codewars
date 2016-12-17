-- Greet Me
-- https://www.codewars.com/kata/535474308bb336c9980006f2

module CodeWars.Kata.GreetMe where
import Data.Char (toUpper, toLower)

greet :: String -> String
greet name = "Hello " ++ cap name ++ "!"
    where cap [] = []
          cap (x:xs) = toUpper x : map toLower xs
