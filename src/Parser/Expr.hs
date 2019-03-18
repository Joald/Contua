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
lambda = do
  void (keyword "fn")
  ids <- many identifier
  void (symbol ".")
  ELambda (map EVar ids) <$> expr

emptyList :: Parser Expr
emptyList = do
  void (symbol "[") >> void (symbol "]")
  return $ EListLiteral []

listLiteral :: Parser Expr
listLiteral = do
  void (symbol "[")
  e <- expr
  rest <- many (symbol "," >> expr)
  void (symbol "]")
  return . EListLiteral $ e : rest

ifExpr :: Parser Expr
ifExpr = EIf <$ keyword "if" <*> expr <* keyword "then" <*> expr <* keyword "else" <*> expr

exprTerm :: Parser Expr
exprTerm = choice
  [ try (parens expr)
  , try (EInt <$> lexeme L.decimal)
  , lambda
  , ifExpr
  , try emptyList <|> listLiteral
  , try (EVar <$> identifier) -- needs to be last to not parse keywords as ids
--  , matchExpr
--  , whereExpr
--  , letExpr
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


-- not added yet:


matchExpr :: Parser Expr
matchExpr = do
  void (symbol "match")
  em <- expr
  void (symbol "in")
  pats <- many $ do
    void (symbol "|")
    expr
  return $ EMatch em pats

whereExpr :: Parser Expr
whereExpr = do
  ep <- expr
  void (symbol "where")
  fs <- many funDecl
  return $ EWhere ep (fromList fs)

letExpr :: Parser Expr
letExpr = do
  void (symbol "let")
  var <- identifier
  void (symbol "=")
  e1 <- expr
  void (symbol "in")
  ELet var e1 <$> expr
