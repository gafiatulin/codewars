-- Heron's formula
-- https://www.codewars.com/kata/57aa218e72292d98d500240f

module HeronsFormula where

heron a b c = sqrt (s*(s-a)*(s-b)*(s-c))
    where s = (a + b + c) / 2.0
