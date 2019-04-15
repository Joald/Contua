module Parser.Expr where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import Parser.TypeDefs
import Parser.Utils
import Parser.TypeDecls

funDecl :: Parser FunDecl
funDecl =
  FunDecl
    <$> type_
    <* symbol "::"
    <*> identifier -- function name
    <*> many identifier -- args
    <* symbol "="
    <*> expr -- body
    <* symbol ";"


lambda :: Parser Expr
lambda =
  ELambda
    <$ (keyword "fn" <|> symbol "\\" <|> symbol "λ")
    <*> many identifier
    <* symbol "."
    <*> expr

listLiteral :: Parser Expr
listLiteral = EListLiteral <$> brackets (expr `sepBy` symbol ",")

ifExpr :: Parser Expr
ifExpr =
  EIf
    <$ keyword "if"
    <*> expr
    <* keyword "then"
    <*> expr
    <* keyword "else"
    <*> expr

letExpr :: Parser Expr
letExpr =
  ELet
    <$ keyword "let"
    <*> identifier
    <* symbol "="
    <*> expr
    <* keyword "in"
    <*> expr

matchExpr :: Parser Expr
matchExpr =
  EMatch
    <$ keyword "match"
    <*> expr
    <* keyword "with"
    <*> some ((,) <$ symbol "|" <*> expr <* symbol "=>" <*> expr)

exprTerm :: Parser Expr
exprTerm = choice
  [ try $ parens expr
  , try $ EInt <$> lexeme L.decimal
  , lambda
  , ifExpr
  , listLiteral
  , try $ EVar <$> identifier
  , try $ ETypeName <$> typeName
  , letExpr
  , matchExpr
  ]


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
