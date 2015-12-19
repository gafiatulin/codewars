-- Vasya and System of Equations
-- http://www.codewars.com/kata/556eed2836b302917b0000a3/

module Codewars.Kata.SystemQuadratic where

solution :: Integer -> Integer -> Integer
solution n m = sum [1 | a <-[0..ceiling . sqrt . fromIntegral $ n], b <-[0..ceiling . sqrt . fromIntegral $ m], a^2+b == n && b^2+a == m ]
