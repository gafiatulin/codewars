-- A man, a plan, a canal, Panama!
-- http://www.codewars.com/kata/54b478163b3d7632d10001c6/

module Palindrome where

import Data.Char (toLower, isAlphaNum)
import Control.Arrow ((&&&))

isPalindrome :: String -> Bool
isPalindrome = uncurry (==) . (id &&& reverse) . map toLower . filter isAlphaNum
