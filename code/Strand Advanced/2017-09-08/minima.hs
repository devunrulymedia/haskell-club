import NanoParse
import Data.Char
import Control.Applicative
import Control.Monad

data Expression 
  = Variable String
  | StringLiteral String
  | Call Expression Expression
  deriving (Show)

letter :: Parser Char
letter = satisfy isAlpha

identifier :: Parser String
identifier = do
  initial <- letter
  rest <- many $ satisfy isAlphaNum
  return $ initial:rest

variable :: Parser Expression
variable = Variable <$> identifier

quoted :: Parser String
quoted = do 
  reserved "'"
  content <- many $ noneOf "'"
  reserved "'"
  return content

stringLiteral :: Parser Expression
stringLiteral = StringLiteral <$> quoted


