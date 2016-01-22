-- Gaussian primes
-- http://www.codewars.com/kata/54d6abf84a35017d30000b26/

module Data.Complex.Gaussian.Prime where

import Control.Arrow ((&&&))
import Data.Complex.Gaussian (Gaussian (..), norm)

isGaussianPrime :: Gaussian -> Bool
isGaussianPrime g@(Gaussian a b) | a == 0 = f b
                                 | b == 0 = f a
                                 | otherwise = isPrime . norm $ g
                                 where f = uncurry (&&) . ((==3) . (`mod` 4) &&& isPrime) . abs
                                       isPrime n | n `elem` [2, 3, 5] = True
                                                 | n `mod` 2 == 0 || n `mod` 3 == 0 = False
                                                 | otherwise = notElem 0 . map (n `mod`) $ [5, 7 .. floor . sqrt . fromIntegral $ n]
