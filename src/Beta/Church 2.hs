-- Church Numbers - Add, Multiply, Exponents
-- http://www.codewars.com/kata/55c0c452de0056d7d800004d/

module Haskell.Codewars.Church where

type Lambda a = (a -> a)
type Cnum a = Lambda a -> Lambda a

churchAdd :: Cnum a -> Cnum a -> Cnum a
churchAdd c1 c2 f = c1 f . c2 f

churchMul :: Cnum a -> Cnum a -> Cnum a
churchMul c1 c2 = c1 . c2

churchPow :: Cnum a -> (Cnum a -> Cnum a) -> Cnum a
churchPow cb ce = ce cb
