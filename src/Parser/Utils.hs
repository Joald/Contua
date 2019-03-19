module Parser.Utils where

import           Control.Monad.Combinators.Expr
import           Data.Void
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

sc_ :: Parser () -> Parser ()
sc_ x = L.space x (L.skipLineComment "#") (L.skipBlockComment "#{" "}#")

sc :: Parser ()
sc = sc_ space1

scDecl :: Parser ()
scDecl = sc_ $ space1 >> eol >> return ()

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

--decl :: Parser a -> Parser a


parens :: Parser a -> Parser a
parens = symbol "(" `between` symbol ")"

brackets :: Parser a -> Parser a
brackets = symbol "[" `between` symbol "]"

op :: String -> Parser String
op n = lexeme $ try $ string n <* notFollowedBy opChar

opChar :: Parser Char
opChar = oneOf "-*+:"

binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

binaryR :: String -> (a -> a -> a) -> Operator Parser a
binaryR name f = InfixR (f <$ symbol name)

binaryTry :: String -> (a -> a -> a) -> Operator Parser a
binaryTry name f = InfixL (f <$ try (symbol name))

prefix, postfix :: String -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)

postfix name f = Postfix (f <$ symbol name)

keywords = ["type", "fn", "let", "in", "match", "with", "if", "then", "else", "and", "or", "not"]

withPredicate :: (a -> Bool) -- ^ The check to perform on parsed input
  -> String -- ^ Message to print when the check fails
  -> Parser a -- ^ Parser to run
  -> Parser a -- ^ Resulting parser that performs the check
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
identifier =
  withPredicate
    (`notElem` keywords)
    "Expected identifier, found keyword."
    (lexeme ((:) <$> lowerChar <*> many alphaNumChar <?> "identifier"))

typeName :: Parser String
typeName =
  withPredicate
    (`notElem` keywords)
    "Expected typename, found keyword."
    (lexeme ((:) <$> upperChar <*> many alphaNumChar <?> "typename"))
