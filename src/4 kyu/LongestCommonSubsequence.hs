-- Longest Common Subsequence
-- http://www.codewars.com/kata/52756e5ad454534f220001ef/

module LongestCommonSubsequence where

import Data.List (subsequences, intersect, maximumBy)
import Data.Ord (comparing)

lcs :: String -> String -> String
lcs x y = maximumBy (comparing length) . intersect (subsequences x) $ (subsequences y)
