-- Twice linear
-- http://www.codewars.com/kata/5672682212c8ecf83e000050/

module Codewars.G964.DblLinear where 

dblLinear :: Int -> Integer
dblLinear n = f !! n
    where f = 1 : merge (map (\x -> 2*x+1) f) (map (\x -> 3*x+1) f)
          merge (x:xs) (y:ys) = case compare x y of 
              EQ -> x : merge xs ys
              LT -> x : merge xs (y:ys)
              GT -> y : merge (x:xs) ys
