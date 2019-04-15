module Main (main) where

import Parser.Parser (ParserError, parseProgram, composeASTs)
import Parser.TypeDefs (fnName, AST(AST), typeDecls)
import TypeSystem.PatternChecker (PatternError, runCoverageCheck, checkPatterns)
import TypeSystem.TypeDefs (TypeSystemError)
import System.Environment (getArgs)
import Data.Bifunctor (first)
import Semantics.Builtins (makePrelude)
import TypeSystem.Preprocessor (preprocess)
import TypeSystem.TypeSystem (typeMapFromDeclList, typeCheck)

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