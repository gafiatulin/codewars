-- Make a function that does arithmetic!
-- https://www.codewars.com/kata/583f158ea20cfcbeb400000a

module Kata where

data Operation = Add | Divide | Multiply | Subtract deriving (Eq, Show, Enum, Bounded)

arithmetic :: Fractional a => a -> a -> Operation -> a
arithmetic a b Add = a + b
arithmetic a b Divide = a / b
arithmetic a b Multiply = a * b
arithmetic a b Subtract = a - b
