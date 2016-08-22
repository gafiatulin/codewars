-- Remove exclamation marks
-- https://www.codewars.com/kata/57a0885cbb9944e24c00008e

module RemoveExclamationMarks where

removeExclamationMarks :: String -> String
removeExclamationMarks = filter ('!' /=)
