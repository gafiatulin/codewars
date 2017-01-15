-- Simple beads count
-- https://www.codewars.com/kata/58712dfa5c538b6fc7000569

module Kata (countRedBeads) where

countRedBeads :: Int -> Int
countRedBeads = max 0 . (*2) . pred
