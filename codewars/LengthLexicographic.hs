-- Sorting lists by length and lexicographically
-- http://www.codewars.com/kata/542712c3a16825621e000b65/

module LengthLexicographic where

newtype LengthList a = LengthList [a]
 deriving(Show,Eq)
 
instance Ord a => Ord (LengthList a) where
    compare (LengthList []) (LengthList [])       = EQ
    compare (LengthList []) (LengthList (_:_))    = LT
    compare (LengthList (_:_)) (LengthList [])    = GT
    compare (LengthList (x:xs)) (LengthList (y:ys)) = 
        case compare (length xs) (length ys) of
            EQ -> case compare x y of
                EQ -> compare xs ys
                ord -> ord
            ord -> ord
