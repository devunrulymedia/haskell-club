import NanoParse
import Data.Char
import Control.Applicative
import Control.Monad

data Expression 
  = Variable String
  | StringLiteral String
  | Call Expression [Expression]
  | Function [String] Expression
  | Access Expression String
  | Object [(String, Expression)]
  | Group [Expression]
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
quoted = enclosed (many $ noneOf "'") "'" "'"

stringLiteral :: Parser Expression
stringLiteral = StringLiteral <$> quoted

keyValuePair :: Parser (String, Expression)
keyValuePair = do
  key <- identifier
  reserved ":"
  value <- expression
  return (key, value)

object :: Parser Expression
object = Object <$> (enclosed (separated keyValuePair "," <|> return []) "{" "}")

group :: Parser Expression
group = Group <$> (enclosed (separated expression ",") "(" ")")

enclosed :: Parser a -> String -> String -> Parser a
enclosed p open close = do
  reserved open
  content <- p
  reserved close
  return content

separated :: Parser a -> String -> Parser [a]
separated p sep = do { a <- p; rest [a] } 
  where rest a = (do reserved ","
                     next <- p
                     rest (a ++ [next]))
                 <|> return a

call :: Parser (Expression -> Expression)
call = do
  arguments <- enclosed (separated expression "," <|> return []) "[" "]"
  return (flip Call arguments) 

function :: Parser Expression
function = do
  arguments <- enclosed (separated identifier "," <|> return []) "[" "]"
  reserved "=>"
  body <- expression
  return (Function arguments body)

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
expression = (variable <|> stringLiteral <|> function <|> object <|> group) `andMaybe` (access <|> call)

run :: String -> Expression
run = runParser expression

