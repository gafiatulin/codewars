-- Esolang: Ticker
-- https://www.codewars.com/kata/5876e24130b45aaa0c00001d

module Haskell.SylarDoom.Ticker where

import Data.Char (chr)
import qualified Data.Sequence as Seq

interpreter :: String -> String
interpreter = reverse . snd . foldl f ((0, Seq.singleton 0), "") . filter (`elem` "<>*+-/!")
    where f ((i, d), o) c | c == '*' = ((i, d), (:o) . chr . (`mod` 256) $ (if i < 0 || i >= Seq.length d then 0 else Seq.index d i))
                          | c `elem` "<>" = (((if c == '<' then pred else succ) i, d), o)
                          | c `elem` "+-" = ((i, Seq.adjust (if c == '+' then succ else pred) i d), o)
                          | c == '/' = ((i, Seq.update i 0 d), o)
                          | otherwise = ((i, (Seq.|>) d 0), o)
