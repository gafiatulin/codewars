-- Hughes' List
-- http://www.codewars.com/kata/5432d1c4913a65b67d00008d/

module HughesList where

import Data.Monoid

newtype Hughes a = Hughes ([a] -> [a])
instance Monoid (Hughes a) where
    mempty = Hughes id
    mappend (Hughes f) (Hughes g) = Hughes (f . g)

runHughes :: Hughes a -> [a]
runHughes (Hughes k) = k []

mkHughes :: [a] -> Hughes a
mkHughes = Hughes . (++)

------------------------------------------------------------

consDumb :: a -> Hughes a -> Hughes a
consDumb a h = mkHughes (a : runHughes h)

cons :: a -> Hughes a -> Hughes a
cons a h = mappend (Hughes (a:)) h

------------------------------------------------------------

appendDumb :: Hughes a -> Hughes a -> Hughes a
appendDumb a b = mkHughes (runHughes a ++ runHughes b)
  
------------------------------------------------------------

snocDumb :: Hughes a -> a -> Hughes a
snocDumb l a = mkHughes (runHughes l ++ [a])

snoc :: Hughes a -> a -> Hughes a
snoc h a = mappend h (Hughes (a:))