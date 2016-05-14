-- Palindrome for your Dome
-- http://www.codewars.com/kata/53046ceefe87e4905e00072a

module Codewars.Kata.Palindrome where
import Prelude hiding (reverse)

import Data.Char (toLower, isAlphaNum)
import Control.Arrow ((&&&))

isPalindrome :: String -> Bool
isPalindrome = uncurry (==) . (id &&& reverse') . map toLower . filter isAlphaNum
  where reverse' [] = []
        reverse' [x] = [x]
        reverse' (x:xs) = reverse' xs ++ [x]
