module Main where

import Parser.Parser
import System.Environment
import System.IO
import Control.Monad
import Parser.TypeDefs
import Control.Monad.Trans.Maybe
import TypeSystem.TypeDefs
import TypeSystem.TypeSystem
import TypeSystem.Preprocessor
import TypeSystem.PatternChecker
import Semantics.Builtins
import Data.Bifunctor
import Text.Megaparsec

{- | Prints the contents of all files in the arguments. -}
catMain :: IO ()
catMain = getArgs >>= mapM readFile >>= mapM_ print

data ProgramError =
    ParseError ParserError
  | PatternCoverageError PatternError
  | TypeError TypeSystemError
  | SemanticError

instance Show ProgramError where
  show (ParseError err) = "Parse error occured:\n" ++ err
  show (TypeError err) = "Type error occured:\n" ++ show err
  show (SemanticError) = "Runtime error occured:\n"
  show (PatternCoverageError s) = s

showResult :: Show a => Either ProgramError a -> String
showResult = either show show

preludeFileName :: String
preludeFileName = "examples/prelude.cont"

parseRealProgram :: String -> String -> Either ProgramError AST
parseRealProgram name = first ParseError . parseProgram name

composeASTs :: AST -> AST -> AST
(AST types1 fns1) `composeASTs` (AST types2 fns2) = AST (types1 ++ types2) (fns1 ++ fns2)

mapPrelude :: AST -> AST
mapPrelude (AST types fns) = AST types $ map (\fn -> fn { fnName = makePrelude $ fnName fn }) fns

{- | Parses one file, typechecks it and prints the AST. -}
oneFileParser :: IO ()
oneFileParser = do
  fname:_ <- getArgs
  contents <- readFile fname
  preludeContents <- readFile preludeFileName
  putStrLn . showResult $ do
    prelude <- mapPrelude <$> parseRealProgram preludeFileName preludeContents
    ast <- parseRealProgram fname contents
    let full = composeASTs prelude ast
        iast = preprocess full
        typeEnv = typeMapFromDeclList $ typeDecls full
    first PatternCoverageError $ runCoverageCheck typeEnv $ checkPatterns full
    first TypeError $ typeCheck iast


main :: IO ()
main = oneFileParser

--f :: a -> (a -> b) -> b
--f x = x