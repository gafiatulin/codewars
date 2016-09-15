-- Maybe - concat 2 maybe's
-- https://www.codewars.com/kata/57da675dfa96908b16000040

module MaybeConcat where

import Control.Monad(liftM2)

concatMaybe :: Maybe String -> Maybe String -> Maybe String
concatMaybe = liftM2 (\a b -> a ++ " " ++ b)
