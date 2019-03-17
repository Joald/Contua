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
  void (symbol "fn")
  ids <- many identifier
  void (symbol ".")
  ELambda (map EVar ids) <$> expr

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


letExpr :: Parser Expr
letExpr = do
  void (symbol "let")
  var <- identifier
  void (symbol "=")
  e1 <- expr
  void (symbol "in")
  ELet var e1 <$> expr




application :: Parser Expr
application = do
  e1 <- expr
  es <- some expr
  return $ applicationHelper e1 es
    where
      applicationHelper e es
        | [] <- es = e
        | x : xs <- es = applicationHelper (EApply e x) xs



exprTerm :: Parser Expr
exprTerm = choice
  [ try (parens expr)
  , try (EInt <$> lexeme L.decimal)
  , lambda
  , try emptyList <|> listLiteral
  , try (EVar <$> identifier) -- needs to be last to not parse keywords as ids
--  , matchExpr
--  , whereExpr
--  , letExpr
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f =  InfixL  (f <$ symbol name)

prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

op :: String -> Parser String
op n = lexeme $ try $ string n <* notFollowedBy opChar

opChar :: Parser Char
opChar = oneOf "-*+:"

exprOperatorTable :: [[Operator Parser Expr]]
exprOperatorTable =
  [ [ binary "" EApply]
  , [ prefix "-" ENeg ]
  , [ binary "*" EMul ]
  , [ InfixL (EAdd <$ op "+")
    , binary "-" ESub ]
  , [ binary ":" ECons]
  , [ InfixL $ EConcat <$ op "++"]
  ]

expr :: Parser Expr
expr = makeExprParser exprTerm exprOperatorTable
