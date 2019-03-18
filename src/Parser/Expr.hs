module Parser.Expr where

import Control.Monad
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Parser.TypeDefs
import Parser.Utils
import Parser.TypeDecls
import Data.List.NonEmpty (fromList)

funDecl :: Parser FunDecl
funDecl = do
  fnType <- type_
  void (symbol "::")
  name <- identifier
  args <- many identifier
  void (symbol "=")
  e <- expr
  void (symbol ";")
  return $ FunDecl fnType name (map EVar args) e


lambda :: Parser Expr
lambda = ELambda <$ keyword "fn" <*> many (EVar <$> identifier) <* symbol "." <*> expr

emptyList :: Parser Expr
emptyList = symbol "[" >> symbol "]" >> return (EListLiteral [])

listLiteral :: Parser Expr
listLiteral = do
  void (symbol "[")
  e <- expr
  rest <- many (symbol "," >> expr)
  void (symbol "]")
  return . EListLiteral $ e : rest

ifExpr :: Parser Expr
ifExpr = EIf <$ keyword "if" <*> expr <* keyword "then" <*> expr <* keyword "else" <*> expr

letExpr :: Parser Expr
letExpr = ELet <$ keyword "let" <*> expr <* symbol "=" <*> expr  <* keyword "in" <*> expr

matchExpr :: Parser Expr
matchExpr = EMatch <$ keyword "match" <*> expr <* keyword "with" <*> many ((,) <$ symbol "|" <*> expr <* symbol "=>" <*> expr)

exprTerm :: Parser Expr
exprTerm = choice
  [ try (parens expr)
  , try (EInt <$> lexeme L.decimal)
  , lambda
  , ifExpr
  , try emptyList <|> listLiteral
  , try (EVar <$> identifier)
  , letExpr
  , matchExpr
--  , whereExpr
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f =  InfixL  (f <$ symbol name)
binaryTry :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryTry name f =  InfixL  (f <$ try (symbol name))

prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)


exprOperatorTable :: [[Operator Parser Expr]]
exprOperatorTable =
  [ [ binaryTry "" EApply ]
  , [ prefix "-" ENeg ]
  , [ Prefix $ ENot <$ keyword "not" ]
  , [ binary "*" EMul ]
  , [ InfixL $ EAdd <$ op "+"
    , binary "-" ESub ]
  , [ binary ":" ECons ]
  , [ InfixL $ EConcat <$ op "++" ]
  , [ binary "==" EEq ]
  , [ binary "<=" ELeq ]
  , [ InfixL $ EAnd <$ keyword "and" ]
  , [ InfixL $ EOr <$ keyword "or" ]
  ]

expr :: Parser Expr
expr = makeExprParser exprTerm exprOperatorTable
