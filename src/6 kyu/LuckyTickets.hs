-- Lucky Tickets
-- https://www.codewars.com/kata/5843f66613ee50e56c0000b9

module Lucky.JorgeVS.Kata where

import Data.Char(digitToInt)

numberOfLuckyTickets :: (Int,Int) -> Int
numberOfLuckyTickets = length . filter ((\(f, s) -> sum f == sum s) . splitAt 3 . map digitToInt . show) . uncurry enumFromTo
