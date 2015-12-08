-- Sleigh Authentication
-- http://www.codewars.com/kata/52adc142b2651f25a8000643

module SleighAuthentication where

authenticate :: String -> String -> Bool
authenticate "Santa Claus" "Ho Ho Ho!" = True
authenticate _ _ = False
