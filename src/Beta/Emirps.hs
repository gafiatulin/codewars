-- Emirps
-- https://www.codewars.com/kata/55de8eabd9bef5205e0000ba

module Codewars.G964.Emirps (findEmirp) where

import Data.Maybe(listToMaybe, fromMaybe)
import Data.List(unfoldr, genericLength)
import Data.Set (fromDistinctAscList, member)

pfactors prs n = unfoldr (\(ds,n) -> listToMaybe [(x, (dropWhile (< x) ds, div n x)) | x <- takeWhile ((<=n).(^2)) ds ++ [ n | n > 1 ], mod n x == 0]) (prs,n)
primes = 2 : 3 : [x | x <- [5,7..], head (pfactors (tail primes) x) == x]
reversed = read .reverse . show

findEmirp :: Integer -> [Integer]
findEmirp n =  [genericLength empirs, fromMaybe 0 . listToMaybe . reverse $ empirs, sum empirs]
    where empirs = filter (\p -> ((/= p) . reversed $ p) && ((`member` pset) . reversed $ p) ) . takeWhile (<= n) $ plist
          pset = fromDistinctAscList plist
          plist = takeWhile (< upperLimit) primes
          upperLimit = read . ('1':) . replicate (length . show $ n) $ '0'
