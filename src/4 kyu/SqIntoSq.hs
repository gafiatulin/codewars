-- Square into Squares. Protect trees!
-- http://www.codewars.com/kata/54eb33e5bc1a25440d000891/

module Codewars.Kata.SqIntoSq where

import Control.Applicative ((<$>), (<|>))

decompose :: Integer -> Maybe [Integer]
decompose n = reverse <$> f (n ^ 2) (n - 1)
    where f n d | d <= 0 = Nothing
                | d^2 == n = Just [d]
                | otherwise = (d:) <$> f (n - d^2) dd <|> f n (d-1)
                where dd = min (d - 1) . floor . sqrt . fromInteger $ (n - d^2)
