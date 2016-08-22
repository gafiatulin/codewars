-- Replace all items
-- https://www.codewars.com/kata/57ae18c6e298a7a6d5000c7a

module ReplaceAllItems where

replaceAll xs a b = map (\x -> if x == a then b else x) xs
