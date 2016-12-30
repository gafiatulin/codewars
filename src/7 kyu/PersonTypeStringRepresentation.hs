-- Person type - string representation
-- https://www.codewars.com/kata/57c9359540e302d32700013a

module PersonTypeStringRepresentation where

data Person = Person String String
instance Show Person where
  show (Person fn ln) = fn ++ " " ++ ln
