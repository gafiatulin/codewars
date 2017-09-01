-- Numbers of Letters of Numbers
-- https://www.codewars.com/kata/599febdc3f64cd21d8000117

module NumbersOfLettersOfNumbers.Kata (numbersOfLetters) where

import Data.Char (digitToInt)
import Data.List (unfoldr)

numbersOfLetters :: Int -> [String]
numbersOfLetters = unfoldr f . (,) False

f :: (Bool, Int) -> Maybe (String, (Bool, Int))
f (e, n) = let s = concatMap ((["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !!) . digitToInt) . show $ n in if e then Nothing else Just (s, (length s == n, length s))
