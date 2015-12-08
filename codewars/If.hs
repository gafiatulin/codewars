-- The 'if' function
-- http://www.codewars.com/kata/54147087d5c2ebe4f1000805

module If where

_if :: Bool -> a -> a -> a
_if b x y = if b then x else y
