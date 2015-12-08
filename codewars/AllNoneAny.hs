-- All, None & Any
-- http://www.codewars.com/kata/54589f3b52756d34d6000158

module Codewars.Kata.AllNoneAny where

import Prelude hiding (all, any)

all, none, any :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) = (f x) && (all f xs)
any f [] = False
any f (x:xs) = if f x then True else any f xs
none f [] = True
none f (x:xs) = not (f x) && none f xs
