module Parser.TypeDecls where

import Text.Megaparsec
import Control.Monad.Combinators.Expr

import Parser.TypeDefs
import Parser.Utils

typeOperatorTable :: [[Operator Parser Type]]
typeOperatorTable =
  [ [ binary "" TApply ]
  , [ binaryR "->" TArrow ]
  ]

typeTerm :: Parser Type
typeTerm = choice
  [ parens type_
  , TList <$> brackets type_
  , TVar <$> identifier
  , TName <$> typeName
  , TCont <$ symbol "@" <*> optional (braces type_) <*> parens type_
  ]

type_ :: Parser Type
type_ = makeExprParser typeTerm typeOperatorTable <* notFollowedBy (symbol "=")

typeVariant :: Parser TypeVariant
typeVariant = TypeVariant <$> typeName <*> many typeTerm

typeDecl :: Parser TypeDecl
typeDecl =
  TypeDecl
    <$ keyword "type"
    <*> typeName
    <*> many (TVar <$> identifier)
    <* symbol "="
    <*> ((:) <$> typeVariant <*> many (symbol "|" *> typeVariant))
    <* symbol ";"

aliasDecl :: Parser TypeAlias
aliasDecl = Alias <$ keyword "alias" <*> typeName <* symbol "=" <*> type_ <* symbol ";"