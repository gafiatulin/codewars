-- My smallest code interpreter (aka Brainf**k)
-- https://www.codewars.com/kata/526156943dfe7ce06200063e

module Brainfuck (executeString) where

import Data.Char (chr, ord)
import Data.Maybe (mapMaybe)

data BFCommand = GoR | GoL | Inc | Dec | Print | Read | LoopL | LoopR deriving (Eq)
data Tape a = Tape [a] a [a]

type BFSource = [BFCommand]

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs
moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
      where zeros = repeat 0

syntax = [('>', GoR), ('<', GoL), ('+', Inc), ('-', Dec), ('.', Print), (',', Read), ('[', LoopL), (']', LoopR)]

validate :: BFSource -> Maybe BFSource
validate source = if validParentheses . filter (`elem` [LoopL, LoopR]) $ source then Just source else Nothing
    where validParentheses :: BFSource -> Bool
          validParentheses xs = vp xs 0
          vp [] 0 = True
          vp [] _ = False
          vp (LoopL:xs) n = vp xs (n+1)
          vp (LoopR:xs) n | n > 0 = vp xs (n-1)
                          | otherwise = False

parse :: String -> Maybe BFSource
parse = validate . mapMaybe (`lookup` syntax)

executeString :: String -> String -> Maybe String
executeString source input = parse source >>= ( fmap reverse . run input [] emptyTape . bfSource2Tape)
    where bfSource2Tape (b:bs) = Tape [] b bs

toChar :: Int -> Char
toChar = chr . (`mod` 256)

run :: String -> String -> Tape Int -> Tape BFCommand -> Maybe String
run i o d                s @ (Tape _ GoR    _) = advance i o (moveRight d) s
run i o d                s @ (Tape _ GoL    _) = advance i o (moveLeft d) s
run i o     (Tape l p r) s @ (Tape _ Inc    _) = advance i o (Tape l (p+1) r) s
run i o     (Tape l p r) s @ (Tape _ Dec    _) = advance i o (Tape l (p-1) r) s
run i o d @ (Tape _ p _) s @ (Tape _ Print  _) = advance i (toChar p : o) d s
run i o d @ (Tape _ p _) s @ (Tape _ LoopL  _) | p == 0 = seekLoopR i o 0 d s
                                               | otherwise = advance i o d s
run i o d @ (Tape _ p _) s @ (Tape _ LoopR  _) | p /= 0 = seekLoopL i o 0 d s
                                               | otherwise = advance i o d s
run i o d @ (Tape l _ r) s @ (Tape _ Read   _) | null i = Nothing
                                               | otherwise = advance (tail i) o (Tape l (ord . head $ i) r) s
advance :: String -> String -> Tape Int -> Tape BFCommand -> Maybe String
advance _ o _ (Tape _ _ []) = Just o
advance i o d s = run i o d (moveRight s)

seekLoopR :: String -> String ->  Int -> Tape Int -> Tape BFCommand -> Maybe String
seekLoopR i o 1 d s @ (Tape _ LoopR _) = advance i o d s
seekLoopR i o b d s @ (Tape _ LoopR _) = seekLoopR i o (b-1) d (moveRight s)
seekLoopR i o b d s @ (Tape _ LoopL _) = seekLoopR i o (b+1) d (moveRight s)
seekLoopR i o b d s                    = seekLoopR i o b d (moveRight s)

seekLoopL :: String -> String ->  Int -> Tape Int -> Tape BFCommand -> Maybe String
seekLoopL i o 1 d s @ (Tape _ LoopL _) = advance i o d s
seekLoopL i o b d s @ (Tape _ LoopL _) = seekLoopL i o (b-1) d (moveLeft s)
seekLoopL i o b d s @ (Tape _ LoopR _) = seekLoopL i o (b+1) d (moveLeft s)
seekLoopL i o b d s                    = seekLoopL i o b d (moveLeft s)
