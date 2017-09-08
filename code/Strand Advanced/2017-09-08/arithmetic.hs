import NanoParse

import Control.Applicative
import Control.Monad

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving Show

eval :: Expr -> Int
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)
eval (Sub a b) = (eval a) - (eval b)
eval (Lit n) = n

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = infixOp "+" Add <|> infixOp "-" Sub

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

run :: String -> Expr
run = runParser expr

int :: Parser Expr
int = do
  n <- number
  return (Lit n)

evaluate :: String -> Int
evaluate = eval . run


