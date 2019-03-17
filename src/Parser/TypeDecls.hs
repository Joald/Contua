module Parser.TypeDecls where

import Control.Monad
import Text.Megaparsec


import Parser.TypeDefs
import Parser.Utils
import Text.Megaparsec.Char

type_ :: Parser Type
type_ = do
  void (symbol "type")
  return TInt

typeDecl :: Parser TypeDecl
typeDecl = (\x -> TypeDecl x [] TInt) <$> symbol "xd"
