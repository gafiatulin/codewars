-- NATO Phonetic Alphabet
-- http://www.codewars.com/kata/54530f75699b53e558002076

module NATO.Alphabet where

import Data.Char (toUpper)
import Data.Map.Lazy (fromList, (!))

letters =  fromList [
    ('A', "Alpha"),  ('B', "Bravo"),   ('C', "Charlie"),
    ('D', "Delta"),  ('E', "Echo"),    ('F', "Foxtrot"),
    ('G', "Golf"),   ('H', "Hotel"),   ('I', "India"),
    ('J', "Juliett"),('K', "Kilo"),    ('L', "Lima"),
    ('M', "Mike"),   ('N', "November"),('O', "Oscar"),
    ('P', "Papa"),   ('Q', "Quebec"),  ('R', "Romeo"),
    ('S', "Sierra"), ('T', "Tango"),   ('U', "Uniform"),
    ('V', "Victor"), ('W', "Whiskey"), ('X', "X-ray"),
    ('Y', "Yankee"), ('Z', "Zulu")
  ]

nato = unwords . map ((letters !) . toUpper)
