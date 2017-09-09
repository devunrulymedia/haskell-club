import NanoParse
import Data.Char
import Control.Applicative
import Control.Monad

data Expression 
  = Variable String
  | StringLiteral String
  | Call Expression [Expression]
  | Access Expression String
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

list :: Parser a -> String -> String -> String -> Parser [a]
list p open sep close = do
  reserved open
  elements <- separated p sep
  reserved close
  return elements

separated :: Parser a -> String -> Parser [a]
separated p sep = do { a <- p; rest [a] } <|> return [] 
  where rest a = (do reserved ","
                     next <- p
                     rest (a ++ [next]))
                 <|> return a

call :: Parser (Expression -> Expression)
call = do
  arguments <- list expression "[" "," "]"
  return (flip Call arguments) 

access :: Parser (Expression -> Expression)
access = do
  reserved ":"
  field <- identifier
  return (flip Access field)

andMaybe :: Parser a -> Parser (a -> a) -> Parser a
p `andMaybe` op = do { a <- p; rest a }
  where rest a = (do f <- op
                     rest (f a))
                 <|> return a 

expression :: Parser Expression
expression = (variable <|> stringLiteral) `andMaybe` (access <|> call)

run :: String -> Expression
run = runParser expression

