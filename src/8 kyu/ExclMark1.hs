-- Exclamation marks series #1: Remove a exclamation mark from the end of string
-- https://www.codewars.com/kata/57fae964d80daa229d000126

module Kata where

remove :: String -> String
remove "" = ""
remove "!" = ""
remove (x:xs) = x:(remove xs)
