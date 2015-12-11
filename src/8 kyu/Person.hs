-- Broken Greetings
-- http://www.codewars.com/kata/50654ddff44f800200000001

module Person where

data Person = Person { name :: String }

greet :: Person -> String -> String
greet person otherName = "Hi " ++ otherName ++ ", my name is " ++ name person
