-- Alan Partridge I - Partridge Watch
-- https://www.codewars.com/kata/5808c8eff0ed4210de000008

module Codewars.AlanPartridge.PartridgeWatch where

part :: [String] -> String
part l | count <= 0 = "Lynn, I've pierced my foot on a spike!!"
       | otherwise = "Mine's a Pint" ++ replicate count '!'
       where count = length . filter (`elem` ["Partridge", "PearTree", "Chat", "Dan", "Toblerone", "Lynn", "AlphaPapa", "Nomad"]) $ l
