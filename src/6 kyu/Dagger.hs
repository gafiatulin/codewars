module Dagger where

import Control.Monad (liftM, liftM2)

data Dagger
  = Literal  Double
  | Add      Dagger Dagger
  | Multiply Dagger Dagger
  | Absolute Dagger
  | Signum   Dagger
  | Negate   Dagger
  | Divide   Dagger Dagger deriving (Eq, Show)

instance Num Dagger where
    (+) = Add
    (*) = Multiply
    negate = Negate
    abs = Absolute
    signum = Signum
    fromInteger = Literal . fromIntegral

instance Fractional Dagger where
    (/) = Divide
    fromRational = Literal . fromRational

interp :: Dagger -> Maybe Double
interp (Literal a) = Just a
interp (Add a b) = liftM2 (+) (interp a) (interp b)
interp (Multiply a b) = liftM2 (*) (interp a) (interp b)
interp (Absolute a) = liftM (\x -> if x < 0 then negate x else x) . interp $ a
interp (Signum a) = liftM (\x -> case compare x 0 of
    EQ -> 0
    LT -> -1
    GT -> 1) . interp $ a
interp (Negate a) = liftM negate . interp $ a
interp (Divide a b) = interp b >>= (\d -> if d /= 0 then liftM2 (/) (interp a) (interp b) else Nothing)