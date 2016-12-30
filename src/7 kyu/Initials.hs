-- Get initials from person name
-- https://www.codewars.com/kata/57b56af6b69bfcffb80000d7

module  Initials where

toInitials :: String -> String
toInitials = unwords . map ((: ".") . head) . words
