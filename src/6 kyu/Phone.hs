-- Valid Phone Number
-- http://www.codewars.com/kata/525f47c79f2f25a4db000025/

module Codewars.Kata.Phone where

import Data.Char (isDigit)

validPhoneNumber :: String -> Bool
validPhoneNumber ('(':xs) = case splitAt 3 xs of 
    (code, xxs) -> all isDigit code && case xxs of
        (')':' ': xxxs) -> all (\c -> isDigit c || c == '-') xxxs
        _ -> False 
validPhoneNumber _ = False
