-- Finally tagless interpreter
-- https://www.codewars.com/kata/5424e3bc430ca2e577000048

{-# LANGUAGE RankNTypes #-}

module Tagless where

import Data.Function (fix)
import Prelude hiding (and, or)

class Language r where
  here   :: r (a, h) a
  before :: r h a -> r (any, h) a
  lambda :: r (a, h) b -> r h (a -> b)
  apply  :: r h (a -> b) -> (r h a -> r h b)

  loop   :: r h (a -> a) -> r h a

  int    :: Int -> r h Int
  add    :: r h Int -> r h Int -> r h Int
  down   :: r h Int -> r h Int    -- \x -> x - 1
  up     :: r h Int -> r h Int    -- \x -> x + 1
  mult   :: r h Int -> r h Int -> r h Int
  gte    :: r h Int -> r h Int -> r h Bool

  bool   :: Bool -> r h Bool
  and    :: r h Bool -> r h Bool -> r h Bool
  or     :: r h Bool -> r h Bool -> r h Bool
  neg    :: r h Bool -> r h Bool

  ifte   :: r h Bool -> r h a -> r h a -> r h a

newtype L h a = L { run :: h -> a }

arity0 :: a -> L h a
arity0 = L . const

arity1 :: (a -> b) -> L h a -> L h b
arity1 op e = L (op . run e)

arity2 :: (a -> b -> c) -> L h a -> L h b -> L h c
arity2 op e1 e2 = L $ \h -> op (run e1 h) (run e2 h)

instance Language L where
    here     = L fst
    before e = L (run e . snd)
    lambda e = L $ \h x -> run e (x, h)
    loop e = L (fix . run e)
    ifte e1 e2 e3 = L $ \h -> if run e1 h then run e2 h else run e3 h

    int  = arity0
    bool = arity0

    down = arity1 pred
    up   = arity1 succ
    neg  = arity1 not

    add  = arity2 (+)
    mult = arity2 (*)
    gte  = arity2 (>=)
    apply = arity2 ($)
    and  = arity2 (&&)
    or   = arity2 (||)

type Term a = forall r h . Language r => r h a

interpret :: Term a -> a
interpret t = run t ()
