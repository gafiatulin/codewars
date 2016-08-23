-- Thinking & Testing : Incomplete string
-- https://www.codewars.com/kata/56d9292cc11bcc3629000533

module Codewars.ThinkingAndTesting02 where

import Data.Char (ord, chr)

testit :: String -> String
testit [] = []
testit [x] = [x]
testit (x:y:xs) = chr ((ord x + ord y) `div` 2) : testit xs
