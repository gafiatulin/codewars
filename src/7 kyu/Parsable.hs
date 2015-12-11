-- You Shall Not Parse! 
-- http://www.codewars.com/kata/547b51dcd587f852e4000ad6/

module Parsable where

parses :: String -> Bool
parses "" = False
parses [c] = c `elem` ['0'..'9']
parses (x:xs) = (x `elem` '-' : ['0'..'9']) && (all (`elem` ['0'..'9']) xs)
