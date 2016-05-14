-- Dice rolls threshold
-- http://www.codewars.com/kata/55d18ceefdc5aba4290000e5/

module Dice (rollDice) where

import Data.Array (array, Array, (!))
import Data.Ratio ((%))

rollDice :: Integer -> Integer -> Integer -> Rational
rollDice rolls sides threshold = (/ fromIntegral (sides^rolls)) . sum $ [j | (i,j) <- distributions ! (rolls, sides), i >= threshold]

binomial n k = coeffs !! fromIntegral n !! fromIntegral k
coeffs = iterate next [1] 
    where next ns = zipWith (+) (0:ns) $ ns ++ [0]

distributions = array ((0,0), (100, 100)) [ ((j, i), f j i) | i <- [0..100], j <- [0..100]]
    where f rolls sides = map (\s -> (s, sum . map (\k -> (-1)^k * binomial rolls k * binomial (s-k*sides-1) (rolls-1)) $ [0..(s-rolls) `div` sides])) [rolls .. sides*rolls]