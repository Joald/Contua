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

op :: String -> Parser String
op n = lexeme $ try $ string n <* notFollowedBy opChar

opChar :: Parser Char
opChar = oneOf "-*+:"

keywords = ["type", "fn", "let", "in", "match", "with", "if", "then", "else", "and", "or", "not"]

withPredicate
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else do
      setOffset o
      fail msg

keyword :: String -> Parser String
keyword keyword = lexeme $ try $ string keyword <* notFollowedBy alphaNumChar


identifier :: Parser String
identifier = withPredicate (`notElem` keywords) "" (lexeme ((:) <$> lowerChar <*> many alphaNumChar <?> "identifier"))
