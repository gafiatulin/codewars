-- Your Ride Is Here
-- http://www.codewars.com/kata/55491e9e50f2fc92f3000074/

module Codewars.Kata.Ride where

import Codewars.Kata.Ride.Types
import Data.Char (ord)

ride :: String -> String -> Ride
ride a b = if f a == f b then Go else Stay
    where f = (`mod` 47) . product . map (\c -> ord c - ord 'A' + 1)
