-- Interpolation Phalanx
-- http://www.codewars.com/kata/543cb1d8f6b726292d0000d7/

module Phalanx where

import Data.Monoid
import Data.String
import Control.Monad (liftM, liftM2)

type Name = String

data Phalanx
  = Literal String
  | Empty
  | Join Phalanx Phalanx
  | Repeat Int Phalanx
  | Lookup Name

instance IsString Phalanx where
    fromString = Literal

instance Monoid Phalanx where
    mempty = Empty
    mappend = Join

rep :: Int -> Phalanx -> Phalanx
rep = Repeat 

l :: Name -> Phalanx
l = Lookup

interp :: (Name -> Maybe String) -> Phalanx -> Maybe String
interp f Empty = Just ""
interp f (Literal s) = Just s
interp f (Join p1 p2) = liftM2 (++) (interp f p1) (interp f p2)
interp f (Repeat n p) = liftM (concat . replicate n) (interp f p)
interp f (Lookup name) = f name
