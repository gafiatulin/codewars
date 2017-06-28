-- Peano numbers
-- https://www.codewars.com/kata/5779b0f0ec883247b2000117

module Haskell.Codewars.Peano where
import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)

data Peano = Zero | Succ Peano deriving (Eq, Show)

add, sub, mul, div :: Peano -> Peano -> Peano

add p1 Zero = p1
add Zero p2 = p2
add p1 (Succ p2) = add (Succ p1) p2

sub p1 Zero = p1
sub Zero _ = error "negative number"
sub (Succ p1) (Succ p2) = sub p1 p2

mul Zero _ = Zero
mul _ Zero = Zero
mul p1 (Succ Zero) = p1
mul (Succ Zero) p2 = p2
mul p1 (Succ p2) = add p1 (mul p1 p2)

div _ Zero = error "divide by 0"
div p1 (Succ Zero) = p1
div Zero _ = Zero
div p1 p2 | compare p1 p2 == LT = Zero
          | otherwise = Succ (div (sub p1 p2) p2)

even, odd :: Peano -> Bool
even Zero = True
even (Succ p1) = not . even $ p1
odd = not . even

compare :: Peano -> Peano -> Ordering
compare Zero Zero = EQ
compare Zero (Succ _) = LT
compare (Succ _) Zero = GT
compare (Succ p1) (Succ p2) = compare p1 p2
