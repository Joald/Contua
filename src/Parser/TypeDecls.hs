module Parser.TypeDecls where

import Control.Monad
import Text.Megaparsec


import Parser.TypeDefs
import Parser.Utils
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr

typeOperatorTable :: [[Operator Parser Type]]
typeOperatorTable =
  [ [ binary "" TApply ]
  , [ binaryR "->" TArrow ]
  ]

typeTerm :: Parser Type
typeTerm = choice
  [ parens type_
  , TList <$> brackets type_
  , TPoly <$> identifier
  , TCtor <$> typeName
  ]

type_ :: Parser Type
type_ = makeExprParser typeTerm typeOperatorTable

typeVariant :: Parser TypeVariant
typeVariant = TypeVariant <$> typeName <*> many typeTerm

typeDecl :: Parser TypeDecl
typeDecl =
  TypeDecl
    <$ keyword "type"
    <*> typeName
    <*> many (TPoly <$> identifier)
    <* symbol "="
    <*> ((:) <$> typeVariant <*> many (symbol "|" *> typeVariant))
    <* symbol ";"
