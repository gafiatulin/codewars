-- The Arpeggiator
-- http://www.codewars.com/kata/563c823393305ec84e000048/

module Codewars.Kata.Arpeggio where

arpeggio :: Char -> Maybe String
arpeggio n | n `elem` scale = Just( (\s -> [head s, s!!2, s!!4, s!!7]) . dropWhile (<n) . cycle $ scale )
           | otherwise = Nothing
           where scale = ['A'..'G']
