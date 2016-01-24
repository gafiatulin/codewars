-- Regular expression parser
-- http://www.codewars.com/kata/5470c635304c127cad000f0d/

module RegExpParser (RegExp(..), parseRegExp) where

import Control.Applicative (pure, empty, (<|>), (<*>), (<$>) , Applicative, Alternative)
import Control.Monad (ap, (>=>), guard)
import Control.Arrow ((&&&))

data RegExp = Normal Char | Any | ZeroOrMore RegExp | Or RegExp RegExp | Str [RegExp] deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Monad Parser where
    return x = Parser (\s -> Just (x, s))
    (Parser f) >>= g = Parser (f >=> (\(x, s) -> runParser (g x) s))

instance Functor Parser where
    fmap f = ap (return f)

instance Applicative Parser where
    pure = return
    fa <*> xa = fa >>= \f -> xa >>= \x -> return (f x)

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser f) <|> (Parser g) = Parser (uncurry (<|>) . (f &&& g))
    

liftToken :: (Char -> Maybe a) -> Parser a
liftToken f = Parser g
    where g [] = Nothing
          g (c:cs) = f c >>= (\x -> Just (x, cs))

eat :: Char -> Parser ()
eat t = liftToken (guard . (==t))

wrap :: Char -> Parser a -> Char -> Parser a
wrap l p r = eat l >> 
             p >>= 
             \x -> eat r >>
             return x 
            
parseList :: Parser a -> Parser [a]
parseList itemP = listP
    where listP = ((:) <$> itemP <*> listP) <|> return []

(~>) :: Char -> a -> Parser a
c ~> x = eat c >> return x

atomP :: Parser RegExp
atomP = liftToken isChar <|> ('.' ~> Any) <|> wrap '(' regExpP ')'
  where isChar c | c `elem` ".*|()" = Nothing
                 | otherwise        = Just (Normal c)

zeroOrMoreP :: Parser RegExp
zeroOrMoreP = (atomP >>= 
               \a -> eat '*' >> 
               return (ZeroOrMore a)) <|> atomP

seqP :: Parser RegExp
seqP = (zeroOrMoreP >>= 
        \a -> zeroOrMoreP >>= 
        \b -> parseList zeroOrMoreP >>= 
        \c -> return (Str (a:b:c))) <|> zeroOrMoreP
    
regExpP :: Parser RegExp
regExpP = (seqP >>=
           \l -> eat '|' >>
           seqP >>= 
           \r -> return (Or l r)) <|> seqP
    
parseRegExp :: String -> Maybe RegExp
parseRegExp s = case runParser regExpP s of Just (x, "") -> Just x
                                            _            -> Nothing
