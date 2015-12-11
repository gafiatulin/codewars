-- Naughty or Nice?
-- http://www.codewars.com/kata/52a6b34e43c2484ac10000cd

module NaughtyNice where

type Warrior = (String, Bool)

getNiceNames :: [Warrior] -> [String]
getNiceNames = map fst . filter (\(s, n) -> n)

getNaughtyNames :: [Warrior] -> [String]
getNaughtyNames = map fst . filter (\(s, n) -> not n)
