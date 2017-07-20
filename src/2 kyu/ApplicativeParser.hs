-- Writing applicative parsers from scratch
-- http://www.codewars.com/kata/54f1fdb7f29358dd1f00015d/

module ApplicativeParser where

import Prelude hiding (fmap)
import Data.Char (Char, isDigit)
import Control.Arrow (second)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f (P g) = P $ map (second f) . g

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) x = pmap (const x)

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P f
    where f [] = []
          f (c:cs) = [(cs, c) | p c]

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P (\s -> [(s,x)])

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
(P f) <@> (P g) = P (\s -> [(s2, ff v) | (s1, ff) <- f s, (s2, v) <- g s1])

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = (const <#> pa) <@> pb

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = (flip const <#> pa) <@> pb

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP = foldr (\ c -> (<@>) ((:) <#> charP c)) (inject [])

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P . const $ []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(P f) <<>> (P g) = P (\s -> f s ++ g s)

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = inject [] <<>> some p

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = ((:) <#> p) <@> many p

-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser (P f) s = [v | (s', v) <- f s, null s']

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p s = case runParser p s of 
    [x] -> Just x
    _ -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int | BinOpE BinOp Expr Expr | NegE Expr | ZeroE deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr e = case e of
    ZeroE -> 0
    (ConstE x) -> x
    (NegE e) -> negate . evalExpr $ e
    (BinOpE op e1 e2) -> f op (evalExpr e1) (evalExpr e2)
    where f AddBO = (+)
          f MulBO = (*)

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique pExpr
    where pExpr = pConst <<>> pBinOpExpr <<>> pNeg <<>> pZero
          pConst = (ConstE . read) <#> some (predP isDigit)
          pNeg = NegE <#> (charP '-' @> pExpr)
          pZero = ZeroE <# charP 'z'
          pBinOpExpr = (\e1 op e2 -> BinOpE op e1 e2) <#> (charP '(' @> pExpr <@ charP ' ') <@> pBinOp <@> (charP ' ' @> pExpr <@ charP ')')
          pBinOp = (AddBO <# charP '+') <<>> (MulBO <# charP '*')
