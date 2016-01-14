-- Gaussian arithmetic
-- http://www.codewars.com/kata/55182e34a5808b90f4001335/

module Data.Complex.Gaussian.Instances (Gaussian (..)) where

import Data.Complex.Gaussian (Gaussian (..))

instance Num Gaussian where
    (+) (Gaussian a b) (Gaussian c d) = Gaussian (a + c) (b + d)
    (*) (Gaussian a b) (Gaussian c d) = Gaussian (a*c-b*d) (a*d+b*c)
    negate (Gaussian a b) = Gaussian (negate a) (negate b)
    signum (Gaussian 0 0) = Gaussian 0 0
    signum (Gaussian a b) | a  > 0 && b >= 0 = Gaussian   1   0
                          | a <= 0 && b >  0 = Gaussian   0   1
                          | a  < 0 && b <= 0 = Gaussian (-1)  0
                          | a >= 0 && b <  0 = Gaussian   0 (-1)
    abs (Gaussian 0 0) = Gaussian 0 0
    abs (Gaussian a b) | a  > 0 && b >= 0 = Gaussian   a    b
                       | a <= 0 && b >  0 = Gaussian   b  (-a)
                       | a  < 0 && b <= 0 = Gaussian (-a) (-b)
                       | a >= 0 && b <  0 = Gaussian (-b)   a
    fromInteger n = Gaussian n 0

instance Integral Gaussian where
    toInteger (Gaussian a b) = a*a + b*b  
    quotRem (Gaussian a b) (Gaussian c d) = (q, (Gaussian a b) - q * (Gaussian c d))
        where q = Gaussian ((a*c+b*d) `quot` (c*c+d*d)) ((b*c-a*d) `quot` (c*c+d*d))
