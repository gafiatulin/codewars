-- Least Common Multiple
-- http://www.codewars.com/kata/5259acb16021e9d8a60010af/

module LeastCommonMultiple where
import Prelude hiding (lcm)

lcm :: Integral a => [a] -> a
lcm = abs . foldl1 lcm'
    where lcm' a b | a == 0 || b == 0 = 0
                   | otherwise = (* b) . (a `div`) . gcd a $ b
