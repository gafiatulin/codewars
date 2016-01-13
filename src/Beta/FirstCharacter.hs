-- first character that repeats
-- http://www.codewars.com/kata/54f9f4d7c41722304e000bbb/

module Codewars.Kata.FirstCharacter where

firstDup :: Eq a => [a] -> Maybe a
firstDup [] = Nothing
firstDup (x:xs) | x `elem` xs = Just x
                | otherwise = firstDup xs
