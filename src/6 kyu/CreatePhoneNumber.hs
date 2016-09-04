-- Create Phone Number
-- https://www.codewars.com/kata/525f50e3b73515a6db000b83

module CreatePhoneNumber where

import Data.Char (intToDigit)

createPhoneNumber :: [Int] -> String
createPhoneNumber ds | (==10) . length $ s = '(' : take 3 s ++ ") " ++ (take 3 . drop 3 $ s) ++ "-" ++ drop 6 s
                     | otherwise = error "wrong digits"
                     where s = take 10 . map intToDigit . filter (`elem` [0..9]) $ ds
