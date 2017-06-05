module Parser where

import Data.Char
import Control.Applicative
import Control.Monad

data Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p)s = p s


instance Functor Parser where
    fmap f p = P (\s -> case parse p s of
                            Nothing -> Nothing
                            Just (v, t) -> Just (f v, t))

instance Applicative Parser where
    pure v = P (\s -> Just (v, s))
    (<*>) = ap

instance Monad Parser where
    return v =  P (\s -> Just (v, s))
    p >>= f =  P(\s -> case parse p s of
                         Nothing -> Nothing
                         Just (v, t) -> parse (f v) t)
 

atom :: Parser Char
atom = P(\s -> case s of 
                 [] -> Nothing
                 (x:xs) -> Just (x, xs))

infixr 5 +++
(+++) :: Parser a -> Parser a -> Parser a
p +++ q  = P (\s -> case parse p s of
                     Nothing -> parse q s
                     Just (v, t) -> Just (v, t))

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- atom
           if p x then return x else P(\_ -> Nothing)

digit, lower, upper, letter, alphanum :: Parser Char
digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

zeroOrMore, oneOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p +++ return []
oneOrMore p = do v <- p
                 vs <- zeroOrMore p
                 return (v: vs)

ident :: Parser String
ident = do x <- lower
           xs <- zeroOrMore alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- oneOrMore digit
         return (read xs)

space :: Parser ()
space = do zeroOrMore (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

