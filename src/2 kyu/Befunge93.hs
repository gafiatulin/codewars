-- Befunge Interpreter
-- https://www.codewars.com/kata/526c7b931666d07889000a3c

module Befunge93 (interpret) where

import System.Random (StdGen, next)
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt, ord, chr)
import qualified Data.Map.Strict as Map

data MoveDirection = MoveUp | MoveDown | MoveLeft | MoveRight deriving (Enum)
data BFState = BFState {rGen:: StdGen, sourceMap :: Map.Map (Int, Int) Char, stack :: [Int], direction :: MoveDirection, output :: String, stringMode :: Bool, size :: (Int, Int), position :: (Int, Int)}

charToDir '>' = MoveRight
charToDir '<' = MoveLeft
charToDir '^' = MoveUp
charToDir 'v' = MoveDown

commandToF '+' = (+)
commandToF '-' = (-)
commandToF '*' = (*)
commandToF '/' = \x y -> if y == 0 then 0 else x `div` y
commandToF '%' = \x y -> if y == 0 then 0 else x `mod` y
commandToF '`' = \x y -> if y < x then 1 else 0

double [] = [0]
double (x:xs) = x:x:xs

swap [x] = [0, x]
swap (x:y:xs) = y:x:xs

put st = let (y:x:v:xs) = stack st in st {stack = xs, sourceMap = Map.insert (y, x) (chr v) . sourceMap $ st }

get st = st {stack = v:xs}
    where (y:x:xs) = stack st
          v = ord . (Map.! (y, x)) . sourceMap $ st

mutateStack f st = st {stack = f . stack $ st}

move l st @ (BFState _ _ _ dir _ _ size pos) = st {position = f dir }
    where f MoveUp = ((`mod` fst size) . (+ negate l) . fst $ pos, snd pos)
          f MoveDown = ((`mod` fst size) . (+ l) . fst $ pos, snd pos)
          f MoveLeft = (fst pos, (`mod` snd size) . (+ negate l)  . snd $ pos)
          f MoveRight = (fst pos, (`mod` snd size) . (+ l)  . snd $ pos)

stackToOutput f st = let (x:xs) = stack st in st {stack = xs, output = output st ++ f x}

ff c st | c `elem` ['0'..'9'] = runState . move 1 . mutateStack (digitToInt c:) $ st
        | c `elem` "+-*/%`" = runState . move 1 . mutateStack (\(a:b:xs) -> commandToF c b a : xs) $ st
        | c `elem` "><^v" = runState . move 1 $ st {direction = charToDir c}
        | c == '"'  = runState . move 1 $ st {stringMode = True}
        | c == ':'  = runState . move 1 . mutateStack double $ st
        | c == '\\' = runState . move 1 . mutateStack swap $ st
        | c == '$'  = runState . move 1 . mutateStack tail $ st
        | c == '.'  = runState . move 1 . stackToOutput show $ st
        | c == ','  = runState . move 1 . stackToOutput ((:[]) . chr) $ st
        | c == '#'  = runState . move 2 $ st
        | c == 'p'  = runState . move 1 . put $ st
        | c == 'g'  = runState . move 1 . get $ st
        | c `elem` "|_?"  = runState . move 1 . chooseDir c $ st
        | c == '!'  = runState . move 1 . mutateStack (\(x:xs) -> (if x == 0 then 1 else 0):xs) $ st
        | c == '@'  = output st
        | otherwise  = runState . move 1 $ st

chooseDir '_' st = let (x:xs) = stack st in st {stack = xs, direction = if x == 0 then MoveRight else MoveLeft}
chooseDir '|' st = let (x:xs) = stack st in st {stack = xs, direction = if x == 0 then MoveDown else MoveUp}
chooseDir '?' st = let (rand, g) = next (rGen st) in st {rGen = g, direction = toEnum (rand `mod` 4)}

runState st | stringMode st = if command == '"' then runState . move 1 $ st {stringMode = False} else runState . move 1 . mutateStack (ord command:) $ st
            | otherwise = ff command st
            where command = fromMaybe ' ' . Map.lookup (position st) . sourceMap $ st

interpret :: StdGen -> String -> String
interpret gen source = let ss = lines source in runState BFState {
    rGen = gen,
    sourceMap = Map.fromList . concatMap (\(i, s) -> zipWith (\j c -> ((i, j), c)) [0..] s) . zip [0..] $ ss,
    stack = [],
    direction = MoveRight,
    output = "",
    stringMode = False,
    size = (length ss, maximum . map length $ ss),
    position = (0, 0)
}
