-- Singletons
-- http://www.codewars.com/kata/54750ed320c64c64e20002e2/

{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
    VNil :: Vec a Zero
    VCons :: a -> Vec a n -> Vec a (Succ n)

data Nat = Zero | Succ Nat

data SNat a where
    SZero :: SNat Zero
    SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance Succ m :< Succ n = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add Zero m = m
type instance Add (Succ n) m = Succ (Add n m)

type family (Subtract (a :: Nat) (b :: Nat)) :: Nat
type instance Subtract m Zero = m
type instance Subtract Zero m = Zero
type instance Subtract (Succ n) (Succ m) = Subtract n m

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance Min Zero m = Zero
type instance Min m Zero = Zero
type instance Min (Succ m) (Succ n) = Succ (Min m n)

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons v _) = v
index (SSucc n) (VCons _ xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate _ SZero = VNil
replicate v (SSucc m) = VCons v (replicate v m)

zipWith :: (x -> y -> z) -> Vec x a -> Vec y a -> Vec z a
zipWith _ VNil VNil = VNil
zipWith f (VCons a v1) (VCons b v2) = VCons (f a b) (zipWith f v1 v2)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ a = a
(VCons v m) ++ b = VCons v (m++b)

take :: SNat a -> Vec v m -> Vec v (Min a m)
take SZero _ = VNil
take _ VNil = VNil
take (SSucc a) (VCons v m) = VCons v (take a m)

drop :: SNat a -> Vec v m -> Vec v (Subtract m a)
drop SZero a = a
drop _ VNil = VNil
drop (SSucc a) (VCons v m) = drop a m

head :: Vec v (Succ a) -> v
head (VCons v _) = v

tail :: Vec v (Succ a) -> Vec v a
tail (VCons _ m) = m
