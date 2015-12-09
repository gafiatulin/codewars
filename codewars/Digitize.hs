-- Digitize
-- http://www.codewars.com/kata/5417423f9e2e6c2f040002ae/

module Digitize where

import Data.Char (digitToInt)

digitize :: Integer -> [Integer]
digitize = map (toInteger . digitToInt) . show
