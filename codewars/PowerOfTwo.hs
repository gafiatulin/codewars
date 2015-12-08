-- Power of two
-- http://www.codewars.com/kata/534d0a229345375d520006a0

module Codewars.Kata.PowerOfTwo where

import Data.Bits ((.&.))

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = (n /= 0) && ((.&.) n (n - 1) == 0)
