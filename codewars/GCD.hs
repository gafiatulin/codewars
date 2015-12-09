-- Greatest common divisor
-- http://www.codewars.com/kata/5500d54c2ebe0a8e8a0003fd/

module Codewars.Kata.GCD where

import Prelude hiding (gcd, lcm)

gcd :: Integral n => n -> n -> n
gcd a b | a == b = a
        | b == 0 = a
        | a == 0 = b
        | b > 0 = gcd b (a`mod` b)
