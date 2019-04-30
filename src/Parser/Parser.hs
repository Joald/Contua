module Parser.Parser
    ( parseProgram
    , ParserError
    , program
    , Parser
    , composeASTs
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


composeASTs :: AST -> AST -> AST
(AST types1 aliases1 fns1) `composeASTs` (AST types2 aliases2 fns2) =
  AST (types1 ++ types2) (aliases1 ++ aliases2) (fns1 ++ fns2)

step :: Parser AST
step = choice
  [ AST . (:[]) <$> try typeDecl <*> pure [] <*> pure []
  , AST [] . (:[]) <$> try aliasDecl <*> pure []
  , AST [] [] . (:[]) <$> funDecl
  ]

program :: Parser AST
program = sc *> (foldr composeASTs (AST [] [] []) <$> many step) <* eof

type ParserError = String

parseProgram :: String -> String -> Either ParserError AST
parseProgram fname contents = first errorBundlePretty $ parse program fname contents



