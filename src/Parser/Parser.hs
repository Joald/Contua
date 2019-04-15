module Parser.Parser
    ( parseProgram
    , ParserError
    , program
    , Parser
    ) where

import Text.Megaparsec hiding (State)
import Data.Bifunctor (first)

import Parser.Utils
import Parser.TypeDefs
import Parser.Expr
import Parser.TypeDecls


{- | This is the Contua language, the parser part.
  Much of it, especially the Utils module has been taken directly from:
  https://markkarpov.com/megaparsec/megaparsec.html
-}


program :: Parser AST
program = AST <$ sc <*> many typeDecl <*> many funDecl <* eof

type ParserError = String

parseProgram :: String -> String -> Either ParserError AST
parseProgram fname contents = first errorBundlePretty $ parse program fname contents



