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
(AST types1 fns1) `composeASTs` (AST types2 fns2) = AST (types1 ++ types2) (fns1 ++ fns2)

step :: Parser AST
step = flip AST [] . (:[]) <$> try typeDecl <|> AST [] .  (:[]) <$> funDecl

program :: Parser AST
program = sc *> (foldr1 composeASTs <$> many step) <* eof

type ParserError = String

parseProgram :: String -> String -> Either ParserError AST
parseProgram fname contents = first errorBundlePretty $ parse program fname contents



