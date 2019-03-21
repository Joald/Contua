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


{- | This is the Contua language, the parser part.
  Much of it, especially the Utils module has been taken directly from:
  https://markkarpov.com/megaparsec/megaparsec.html
-}


program :: Parser AST
program = AST <$> many typeDecl <*> many funDecl <* eof

--parseProgram :: String -> String -> Either
parseProgram = parse program



