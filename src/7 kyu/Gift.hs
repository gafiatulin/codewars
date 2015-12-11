-- A Gift Well Spent
-- http://www.codewars.com/kata/54554846126a002d5b000854/

module Gift where

buy :: (Num a, Eq a) => a -> [a] -> Maybe (Int, Int)
buy c is = if null cs then Nothing else Just (head cs)
    where cs = [(i, j) | i<-[0..length is - 1], j <-  drop i [1..length is - 1], is!!i + is!!j == c]
