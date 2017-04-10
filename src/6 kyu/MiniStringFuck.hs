-- Esolang Interpreters #1 - Introduction to Esolangs and My First Interpreter (MiniStringFuck)
-- https://www.codewars.com/kata/586dd26a69b6fd46dd0000c0

module MiniStringFuck where

import Data.Char (chr)

myFirstInterpreter :: String -> String
myFirstInterpreter = reverse . snd . foldl (\(m, o) c -> if c == '.' then (m, chr m : o) else ((`mod` 256) . succ $ m, o)) (0, "") . filter (`elem` ".+")
