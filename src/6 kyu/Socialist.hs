-- Socialist distribution
-- https://www.codewars.com/kata/58cfa5bd1c694fe474000146

module Codewars.Kata.Socialist where

import qualified Data.Sequence as Seq
import Data.Foldable (toList)

type Money = Int

socialistDistribution :: [Money] -> Money -> [Money]
socialistDistribution l = toList . sd (Seq.fromList l)

sd :: Seq.Seq Money -> Money -> Seq.Seq Money
sd distribution povertyLine | Seq.null distribution || ((> s) . (* povertyLine) . Seq.length $ distribution) = Seq.empty
                            | m >= povertyLine = distribution
                            | otherwise = sd (Seq.adjust pred mi' . Seq.adjust succ mi $ distribution) povertyLine
                            where (m, mi, _, mi', s) = Seq.foldlWithIndex f (maxBound :: Int, 0, minBound :: Int , 0, 0) distribution
                                  f (m, mi, m', mi', s) i e | e < m && e > m' = (e, i, e, i, s + e)
                                                            | e < m = (e, i, m', mi', s + e)
                                                            | e > m' = (m, mi, e, i, s + e)
                                                            | otherwise = (m, mi, m', mi', s + e)
