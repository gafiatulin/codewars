-- Algebraic Isomorphism
-- https://www.codewars.com/kata/5917f22dd2563a36a200009c

module ISO where

import Data.Void
import Data.Maybe(isJust)
import Data.Tuple(swap)
import Control.Arrow ((***), (+++), first)

type ISO a b = (a -> b, b -> a)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

refl :: ISO a a
refl = (id, id)

symm :: ISO a b -> ISO b a
symm = swap

trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = (ab *** cd, ba *** dc)

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (fmap ab, fmap ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (ab +++ cd, ba +++ dc)

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) =  (\f -> cd . f . ba, \f -> dc . f . ab)

isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe m@(mamb, mbma) = (get . mamb . Just, substL $ isoUnMaybe $ symm m)
    where get (Just b) = b
          get Nothing = getJust (mamb Nothing)
          getJust (Just b) = b
          getJust Nothing = undefined

isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (symm, symm)

isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoProd = isoTuple

isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoPlus = isoEither

isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
isoS = isoMaybe

isoPow :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoPow = isoFunc

plusComm :: ISO (Either a b) (Either b a)
plusComm = (either Right Left, either Right Left)

plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
plusAssoc = (either (fmap Left) (Right . Right), either (Left . Left) (either (Left . Right) Right))

multComm :: ISO (a, b) (b, a)
multComm = (swap, swap)

multAssoc :: ISO ((a, b), c) (a, (b, c))
multAssoc = (\((a, b), c) -> (a, (b, c)), \(a, (b, c)) -> ((a, b), c))

dist :: ISO (a, Either b c) (Either (a, b) (a, c))
dist = (ae, ea)
    where ae (a, Left b) = Left (a, b)
          ae (a, Right c) = Right (a, c)
          ea (Left  (a, b)) = (a, Left  b)
          ea (Right (a, c)) = (a, Right c)

curryISO :: ISO (a -> b -> c) ((a, b) -> c)
curryISO = (uncurry, curry)

one :: ISO () (Maybe Void)
one = (const Nothing, const ())

two :: ISO Bool (Maybe (Maybe Void))
two = (\b -> if b then Just Nothing else Nothing, isJust)

plusO :: ISO (Either Void b) b
plusO = (left, Right)
    where left (Left x) = absurd x
          left (Right x) = x

plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
plusS = (left, right)
    where left (Left (Just a)) = Just (Left a)
          left (Left Nothing) = Nothing
          left (Right b) = Just (Right b)
          right Nothing = Left Nothing
          right (Just (Left a)) = Left (Just a)
          right (Just (Right b)) = Right b

plusSO :: ISO (Either () b) (Maybe b)
plusSO = isoPlus one refl `trans` plusS `trans` isoS plusO

multO :: ISO (Void, a) Void
multO = (absurd . fst, absurd)

multS :: ISO (Maybe a, b) (Either b (a, b))
multS = (left, either ((,) Nothing) (first Just))
    where left (Nothing, b) = Left b
          left (Just a, b) = Right (a, b)

multSO :: ISO ((), b) b
multSO = isoProd one refl `trans` multS `trans` isoPlus refl multO `trans` plusComm `trans` plusO

powO :: ISO (Void -> a) ()
powO = (const (), const absurd)

powS :: ISO (Maybe b -> a) (a, b -> a)
powS = (\f -> (f Nothing, f . Just), uncurry maybe)

powSO :: ISO (() -> a) a
powSO = isoPow one refl `trans` powS `trans` isoProd refl powO `trans` multComm `trans` multSO
