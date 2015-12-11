-- Going to the cinema
-- http://www.codewars.com/kata/562f91ff6a8b77dfe900006e/

module Codewars.G964.Movie where

import Data.List (unfoldr)

movie :: Int -> Int -> Double -> Int
movie card ticket perc = length (unfoldr 
    (\(overallRegular, lastDiscount, overallDiscount) -> 
    if ceiling overallDiscount < floor overallRegular
    then Nothing
    else Just (0, (
        overallRegular + fromIntegral ticket, 
        lastDiscount * perc,
        overallDiscount + lastDiscount * perc
    ))) (0.0, fromIntegral ticket, fromIntegral card))
