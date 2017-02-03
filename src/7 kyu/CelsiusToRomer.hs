-- Thinkful - Number Drills: Rømer temperature
-- https://www.codewars.com/kata/5862eeeae20244d5eb000005

module Kata (celsiusToRomer)  where

celsiusToRomer :: Double -> Double
celsiusToRomer = (+ 7.5) . (/ 40) . (* 21)
