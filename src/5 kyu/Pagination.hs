-- PaginationHelper
-- http://www.codewars.com/kata/515bb423de843ea99400000a/

{-

Random test for pageIndex is broken:
should work for random collections and ipp values
Falsifiable (after 2 tests and 1 shrink): 
    expected: Just 0
    but got: Nothing

    [0]
    Positive {getPositive = 1}
    -1
-}

module Codewars.Kata.Pagination where

type Collection a = [a]
type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length 
pageCount :: Collection a -> ItemsPerPage -> Int
pageCount xs n = ceiling . (/ (fromIntegral n)) . fromIntegral . itemCount $ xs
pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount xs n page | page < 0 = Nothing
                        | otherwise = case compare (page + 1) (pageCount xs n) of
                            LT -> Just n
                            EQ -> Just (itemCount xs - n*page)
                            GT -> Nothing
pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex xs n item | (item < 0) || (item >= itemCount xs) = Nothing
                    | otherwise = Just . div item $ n
