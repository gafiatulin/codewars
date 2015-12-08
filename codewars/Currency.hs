-- Converting integer to currency format
-- http://www.codewars.com/kata/54e9554c92ad5650fe00022b

module Codewars.Kata.Currency where

toCurrency :: Integer -> String
toCurrency =  reverse. f . reverse . show
    where f (x1:x2:x3:x4:xs) = x1:x2:x3:',':f (x4:xs)
          f x = x
