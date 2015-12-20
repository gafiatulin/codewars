-- Common Bit Twiddles
-- http://www.codewars.com/kata/542332c8c2cf7ccbbb000005/

module BitTwiddles where
import Prelude hiding ((*), (/), (^), (&&), (||), fromIntegral, fromInteger, mod, div, quot, rem, quotRem, divMod, (>), (<), compare, Ordering(..), abs, even, odd)
import Data.Bits hiding (popCount)
import Data.Int (Int32)

isEven :: Bits a => a -> Bool
isEven = not . isOdd

isOdd  :: Bits a => a -> Bool
isOdd = flip testBit 0

halfAndFloor  :: Bits a => a -> a
halfAndFloor = flip shiftR 1

isPowerOfTwo :: (Num a, Bits a) => a -> Bool
isPowerOfTwo x =  (==x) . (.&. x) . (+1) . complement $ x

nthPowerOfTwo :: (Num a, Bits a) => Int -> a
nthPowerOfTwo = shiftL 1

abs :: Int32 -> Int32
abs x = (x `xor` (x `shiftR` 31)) - (x `shiftR` 31)
