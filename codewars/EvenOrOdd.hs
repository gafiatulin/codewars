-- Even or Odd
-- http://www.codewars.com/kata/53da3dbb4a5168369a0000fe

module EvenOrOdd where

evenOrOdd :: Integral a => a -> [Char]
evenOrOdd n | even n = "Even"
            | otherwise = "Odd"
