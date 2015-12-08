-- Jenny's secret message
-- http://www.codewars.com/kata/55225023e1be1ec8bc000390

module Codewars.Kata.Jenny where

greet :: String -> String
greet "Johnny" = "Hello, my love!"
greet name     = "Hello, " ++ name ++ "!"
