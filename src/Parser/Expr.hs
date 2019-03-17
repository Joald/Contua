module Parser.Expr where

import Control.Monad
import Control.Monad.Combinators.Expr
import Text.Megaparsec
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
  return $ FunDecl fnType name args e


lambda :: Parser Expr
lambda = do
  void (symbol "fn")
  ids <- many identifier
  void (symbol ".")
  ELambda ids <$> expr

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

listExpr :: Parser List
listExpr = choice
  [ LVar <$> identifier
  , brackets listLiteral
  , listCons
  , listConcat
  ]

listLiteral :: Parser List
listLiteral = do
  e <- expr
  rest <- many (symbol "," >> expr)
  return . LLiteral $  e : rest


listCons :: Parser List
listCons = do
  e <- expr
  void (symbol ":")
  LCons e <$> listExpr

listConcat :: Parser List
listConcat = do
  l <- listExpr
  ls <- symbol "++" >> (listExpr <|> listConcat)
  return $ LConcat l ls


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
  , try (EVar <$> identifier) -- needs to be last to not parse keywords as ids
--  , matchExpr
--  , whereExpr
--  , letExpr
--  , EList <$> listExpr
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

exprOperatorTable :: [[Operator Parser Expr]]
exprOperatorTable =
  [ [ binary "" EApply]
  , [ prefix "-" ENeg ]
  , [ binary "*" EMul ]
  , [ binary "+" EAdd
    , binary "-" ESub ]
  ]

expr :: Parser Expr
expr = makeExprParser exprTerm exprOperatorTable
