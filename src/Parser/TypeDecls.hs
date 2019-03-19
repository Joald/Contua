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
  , [ binaryR "->" TFun ]
  ]

typeTerm :: Parser Type
typeTerm = choice
  [ TAbstract <$> identifier
  , TCtor <$> typeName
  ]

type_ :: Parser Type
type_ = makeExprParser typeTerm typeOperatorTable

typeDecl :: Parser TypeDecl
typeDecl = flip (flip TypeDecl []) (TCtor "XD") <$> symbol "xd"
