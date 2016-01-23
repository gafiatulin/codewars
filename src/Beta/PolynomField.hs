-- Constructing finite fields using binary polynoms
-- http://www.codewars.com/kata/54f1b7b3f58ba8ee720005a8/

module PolynomField where

import Data.Word (Word)
import Data.Bits (bit, bitSize, setBit, testBit, shift, xor)
import Data.List (intercalate)

newtype BinaryPolynom = BinaryPolynom Word deriving (Eq)

zero, one :: BinaryPolynom
zero = BinaryPolynom 0
one  = BinaryPolynom 1

deg :: BinaryPolynom -> Int
deg (BinaryPolynom n) = bitSize n - countLeadingZeros n - 1
    where countLeadingZeros x = (w-1) - f (w-1)
              where w = bitSize x
                    f i | i < 0 = i
                        | testBit x i = i
                        | otherwise = f (i-1)
{-
Since 4.8.0.0 should be:
import Data.Bits (finiteBitSize, countLeadingZeros) 
deg (BinaryPolynom n) = finiteBitSize n - countLeadingZeros n - 1
-}

polyFromDeg :: Int -> BinaryPolynom
polyFromDeg (-1) = zero
polyFromDeg n = BinaryPolynom . bit $ n

polyFromPowers :: [Int] -> BinaryPolynom
polyFromPowers = BinaryPolynom . foldl setBit (0::Word)

instance Show BinaryPolynom where
    show (BinaryPolynom 0) = "0"
    show bp@(BinaryPolynom n) = intercalate " + " $ ["x^" ++ show i | i <- [m, pred m .. 1], testBit n i] ++ ["1" | testBit n 0]
        where m = deg bp

multiply :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
multiply bp@(BinaryPolynom n) (BinaryPolynom m) = BinaryPolynom . foldl xor 0 . map (shift m) $ [i | i <- [0..deg bp], testBit n i]

(.+.), (.*.) :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
(BinaryPolynom n) .+. (BinaryPolynom m) = BinaryPolynom (n `xor` m)
x .*. y = snd . polyDivMod (multiply x y) . polyFromPowers $ [8, 4, 3, 1, 0]

polyDivMod :: BinaryPolynom -> BinaryPolynom -> (BinaryPolynom, BinaryPolynom)
polyDivMod bp1@(BinaryPolynom n) bp2@(BinaryPolynom m) = case compare (deg bp1) (deg bp2) of
    LT -> (zero, bp1)
    EQ -> (one, bp1 .+. bp2)
    GT -> (q .+. BinaryPolynom (shift 1 dd), r)
    where (q, r) = polyDivMod (BinaryPolynom (n `xor` shift m dd)) bp2
          dd = deg bp1 - deg bp2
