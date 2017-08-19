-- Welcome to the City
-- https://www.codewars.com/kata/5302d846be2a9189af0001e4

module Welcome where
sayhello :: [String] -> String -> String -> String
sayhello ns c s = "Hello, " ++ unwords ns ++ "! Welcome to " ++ c ++ ", " ++ s ++ "!"
