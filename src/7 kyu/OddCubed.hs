-- Sum of Odd Cubed Numbers
-- https://www.codewars.com/kata/580dda86c40fa6c45f00028a

module OddCubed.JorgeVS.Kata where

oddCubed :: [Int] -> Int
oddCubed = sum . map (^3) . filter odd
