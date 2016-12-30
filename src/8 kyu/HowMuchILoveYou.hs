-- I love you, a little , a lot, passionately ... not at all
-- https://www.codewars.com/kata/57f24e6a18e9fad8eb000296

module Kata (howMuchILoveYou) where

ss = ["I love you", "a little", "a lot", "passionately", "madly", "not at all"]

howMuchILoveYou :: Int -> String
howMuchILoveYou = (ss !!) . (`mod` 6) . pred
