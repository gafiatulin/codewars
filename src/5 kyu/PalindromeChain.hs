-- Palindrome chain length
-- http://www.codewars.com/kata/525f039017c7cd0e1a000a26/

module PalindromeChain where

palindromeChainLength :: Integer -> Integer
palindromeChainLength = fromIntegral . length . takeWhile (not . palindrome) . iterate f
    where f x = (+x) . read . reverse . show $ x
          palindrome x = (== show x) . reverse . show $ x
