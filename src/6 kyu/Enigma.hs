-- The Enigma Machine - Part 1: The Plugboard
-- http://www.codewars.com/kata/5523b97ac8f5025c45000900/

module Codewars.Kata.Enigma where

import Data.List (sort, group, find)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)

plugboard :: String -> Either String (Char -> Char)
plugboard [] = Right id
plugboard xs | (length xs <= 20) && (even . length $ xs) && (length xs == (length . unique $ xs)) = Right plug
             | otherwise = Left "Wrong wiring."
             where unique [] = []
                   unique (x: xs) = x : unique (filter (x/=) xs)
                   pairs [] = []
                   pairs (a:b:c) = (a, b) : pairs c
                   plug x = fromMaybe x . liftM (other x) . find (\(a, b) -> a == x || b == x) . pairs $ xs
                   other e (a, b) | e == a = b
                                  | otherwise = a
