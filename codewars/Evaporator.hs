-- Deodorant Evaporator
-- http://www.codewars.com/kata/5506b230a11c0aeab3000c1f/

module Codewars.Kata.Evaporator where

evaporator :: Double -> Double -> Double -> Integer
evaporator content evap_per_day threshold = ceiling $ logBase (1 - (evap_per_day / 100)) (threshold / 100)
