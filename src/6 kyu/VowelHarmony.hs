-- Hungarian Vowel Harmony (harder)
-- https://www.codewars.com/kata/57fe5b7108d102fede00137a

module Kata (instrumental) where

import Data.Maybe (fromMaybe)
import Data.List (find, isSuffixOf)

ps = [('a', 'á'), ('e', 'é'), ('i', 'í'), ('o', 'ó'),('u', 'ú'), ('ö', 'ő'), ('ü', 'ű')]

fv = "eéiíöőüű"
bv = "aáoóuú"

dgs = [("sz", "ssz"), ("zs", "zzs"), ("cs", "ccs")]

instrumental :: String -> String
instrumental s | (`elem` "eéiíöőüűaáoóuú") . last $ s = let l = fromMaybe (last s) . (`lookup` ps) . last $ s in init s ++ [l] ++ if l `elem` fv then "vel" else "val"
               | otherwise = let fs = (maybe (init s ++ replicate 2 (last s)) ((take (length s - 2) s ++) . snd) . find (\ (dg, _) -> dg `isSuffixOf` s)) dgs in fs ++ (if l' `elem` fv then "el" else "al" )
               where l' = last . filter (`elem` (fv ++ bv)) $ s
