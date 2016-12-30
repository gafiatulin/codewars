-- Fizz Buzz Cuckoo Clock
-- https://www.codewars.com/kata/58485a43d750d23bad0000e6

module Kata where

import Data.Char (isDigit)
import Control.Arrow ((***))

fizzBuzzCuckooClock :: String -> String
fizzBuzzCuckooClock time | m == 0 = unwords . replicate h $ "Cuckoo"
                         | m == 30 = "Cuckoo"
                         | m `mod` 15 == 0 = "Fizz Buzz"
                         | m `mod` 5 == 0 = "Buzz"
                         | m `mod` 3 == 0 = "Fizz"
                         | otherwise = "tick"
                         where (h, m) = ((\t -> if t `mod` 12 == 0 then 12 else t `mod` 12) . read *** read . tail) . span isDigit $ time
