module Parser.Parser
    ( parseProgram
    , program
    ) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Text.Megaparsec hiding (State)
import Data.List.NonEmpty


import Parser.Utils
import Parser.TypeDefs
import Parser.Expr
import Parser.TypeDecls
import Data.Bifunctor (first)


{- | This is the Contua language, the parser part.
  Much of it, especially the Utils module has been taken directly from:
  https://markkarpov.com/megaparsec/megaparsec.html
-}


program :: Parser AST
program = AST <$> many typeDecl <*> many funDecl <* eof

type ParserError = String

parseProgram :: String -> String -> Either ParserError AST
parseProgram fname contents = first show $ parse program fname contents



