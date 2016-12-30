-- Describe a list
-- https://www.codewars.com/kata/57a4a3e653ba3346bc000810

module ParseFloat where

describeList []  = "empty"
describeList [x] = "singleton"
describeList xs  = "longer"
