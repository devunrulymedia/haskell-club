{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParse where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser parser input =
  case parse parser input of
    [(result, [])] -> result
    [(_, rest)]    -> error $ "Parser did not consume " ++ rest
    []             -> error "Parser did not find a valid parse"
    _              -> error "Ambiguous parse"

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] ->     []
    (c:cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s -> 
  case parse p s of 
    []     -> parse q s
    result -> result

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else failure

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

noneOf :: [Char] -> Parser Char
noneOf s = satisfy (not . (flip elem s))

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some digit

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n


