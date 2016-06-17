-- Disguised sequences (II)
-- https://www.codewars.com/kata/56fe17fcc25bf3e19a000292

module Codewars.G964.Disguised2 where

u1 :: Integer -> Integer -> Integer
u1 n p = p * succ n

v1 :: Integer -> Integer -> Integer
v1 n p = p * succ (2 * n)

uEff :: Integer -> Integer -> Integer
uEff = u1

vEff :: Integer -> Integer -> Integer
vEff = v1
