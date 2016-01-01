-- Recover a secret string from random triplets
-- http://www.codewars.com/kata/53f40dff5f9d31b813000774/

module RecoverSecretFromTriplets where

import Data.List (nub, (\\))
import Control.Arrow ((&&&))

recoverSecret :: [String] -> String
recoverSecret = f . nub . concatMap (\[a,b,c] -> [(a,b), (b,c)])
    where f [(a,b)] = [a,b]
          f xs =  (h:) . f . filter ((/= h) . fst ) $ xs
              where h = head . uncurry (\\) . ((nub . map fst) &&& (nub . map snd)) $ xs
