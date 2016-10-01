-- The most imperative functional language?
-- https://www.codewars.com/kata/5453af58e6c920858d000823

module Imperative (def, var, lit, while, (+=), (-=), (*=)) where

import Control.Monad (ap)
import Control.Applicative (liftA)

data S a = W ((Int -> Integer, Int) -> (a, Int -> Integer, Int))
data Var = M Int | Imm Integer

instance Functor S where
    fmap  = liftA

instance Applicative S where
    pure x = W (\(st, p) -> (x, st, p))
    (<*>) = ap

instance Monad S where
    return = pure
    (>>=) (W tr) f = W ((\(x, st', p') -> (\(W tr') -> tr' (st', p')) . f $ x) . tr)

def :: S Var -> Integer
def (W tr) = case tr (const 0, 0) of
    (M v, st, _) -> st v
    (Imm v, _, _) -> v

var :: Integer -> S Var
var v = W $ \(st, p) -> (M p, \x -> if x == p then v else st x, succ p)

lit :: Integer -> Var
lit = Imm

while :: Var -> (Integer -> Bool) -> S () -> S ()
while r f (W tr) = W g
    where g (st, p) = let t = case r of M v -> st v
                                        Imm l -> l
                      in if f t then (\(_, st', p') -> g (st', p')) . tr $ (st, p) else ((), st, p)

op :: (Integer -> Integer -> Integer) -> Var -> Var -> S ()
op f (M v1) (M v2) = W(\(st, p) -> ((), \x -> if x == v1 then f (st x) (st v2) else st x, p))
op f (M v1) (Imm v2) = W(\(st, p) -> ((), \x -> if x == v1 then f (st x) v2 else st x, p))
op _ (Imm _) _ = W(\(st, p) -> ((), st, p))

(+=) = op (+)
(-=) = op (-)
(*=) = op (*)
