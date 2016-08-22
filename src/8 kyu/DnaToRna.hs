-- DNA to RNA Conversion
-- https://www.codewars.com/kata/5556282156230d0e5e000089

module DnaToRna where 

dnaToRna :: String -> String
dnaToRna = map (\c -> if c == 'T' then 'U' else c)
