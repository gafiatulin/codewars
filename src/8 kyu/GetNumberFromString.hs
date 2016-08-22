-- Get number from string
-- https://www.codewars.com/kata/57a37f3cbb99449513000cd8

module GetNumberFromString where
import Data.Char (isDigit)

getNumberFromString :: String -> Int
getNumberFromString = read . filter (isDigit)
