-- Say `Hi` - Person type
-- https://www.codewars.com/kata/57a852c353ba334961001480

module PersonSaysHi where

data Person = Person { firstName :: String, lastName :: String }

sayHi :: Person -> String
sayHi p = "Hi, i'am " ++ firstName p ++ " " ++ lastName p ++ " and is nice to meet You."
