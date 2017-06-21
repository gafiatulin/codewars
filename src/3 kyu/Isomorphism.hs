-- Isomorphism
-- https://www.codewars.com/kata/5922543bf9c15705d0000020

module ISO where

import Data.Void
import Data.Tuple(swap)
import Control.Arrow ((***), (+++))

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

isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (Left . either ( (): ) (const []), ab)
    where ab (Left []) = Right ()
          ab (Left (_:x)) = Left x
          ab (Right v) = absurd v

isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (symm, symm)
