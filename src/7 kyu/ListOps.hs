-- Head, Tail, Init and Last
-- http://www.codewars.com/kata/54592a5052756d5c5d0009c3

module ListOps where

import Prelude hiding (head, tail, init, last)

head (x:_)              =  x
tail (_:xs)             =  xs
init [x]                =  []
init (x:xs)             =  x : init xs
last [x]                =  x
last (_:xs)             =  last xs
