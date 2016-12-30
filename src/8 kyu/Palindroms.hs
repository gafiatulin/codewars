-- Is it a palindrom?
-- https://www.codewars.com/kata/57a1fd2ce298a731b20006a4

module Palindroms where

import Data.Char (toLower)

isPalindrom :: String -> Bool
isPalindrom = f . map toLower
    where f s = (== s) . reverse $ s
