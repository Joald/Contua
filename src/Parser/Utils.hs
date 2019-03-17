module Parser.Utils where

import Data.Void
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "#")
  (L.skipBlockComment "#{" "}#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

identifier :: Parser String
identifier = lexeme ((:) <$> lowerChar <*> many alphaNumChar <?> "identifier")

