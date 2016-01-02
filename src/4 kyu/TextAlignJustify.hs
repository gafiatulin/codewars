-- Text align justify
-- http://www.codewars.com/kata/537e18b6147aa838f600001b/

module TextAlignJustify where

import Data.List (unfoldr, inits)

justify :: String -> Int -> String
justify text width = (unlines . init $ ls) ++ (unwords . words . last $ ls)
    where ls = unfoldr f . words $ text
          f [] = Nothing
          f xs = Just (if length s == 1 then head s else concat . zipWith (++) s $ replicate ll (replicate (l+1) ' ') ++ replicate (length s - (1 + ll)) (replicate l ' ') ++ [""], drop (length s) xs)
              where (l, ll) = (`divMod` (length s - 1)) . (width -) . length . concat $ s
                    s = last . takeWhile (\init -> (<= width) . (+ (pred . length $ init)) . length . concat $ init) . inits $ xs
