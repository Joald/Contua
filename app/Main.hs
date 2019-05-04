module Main (main) where

import Parser.Parser (ParserError, parseProgram, composeASTs)
import Parser.TypeDefs (fnName, mapFromDeclList, AST(..))
import TypeSystem.PatternChecker (PatternError, runCoverageCheck, checkPatterns)
import TypeSystem.TypeDefs (TypeSystemError, IAST, iTypeDecls)
import System.Environment (getArgs)
import Data.Bifunctor (first)
import Semantics.Builtins (makePrelude)
import TypeSystem.Preprocessor (preprocess)
import TypeSystem.TypeSystem (typeCheck)
import Semantics.Interpreter (interpretAST)
import Semantics.TypeDefs
import Debug.Trace (traceM)
import System.IO
import Data.Either (isLeft, isRight)
import Control.Monad (when, unless, zipWithM)
import System.Exit (exitFailure)
import System.Directory

data ProgramError =
    ParseError ParserError
  | PatternCoverageError PatternError
  | TypeError TypeSystemError
  | PreprocessingError String
--  | SemanticError

instance Show ProgramError where
  show (ParseError err) = "Parse error occured:\n" ++ err
  show (TypeError err) = "Type error occured:\n" ++ show err
--  show (SemanticError) = "Runtime error occured:\n"
  show (PatternCoverageError s) = s
  show (PreprocessingError s) = s

showResult :: Show a => Either ProgramError a -> String
showResult = either show show

preludeFileName :: String
preludeFileName = "prelude.cont"

parseRealProgram :: String -> String -> Either ProgramError AST
parseRealProgram name = first ParseError . parseProgram name

mapPrelude :: AST -> AST
mapPrelude (AST types als fns) = AST types als $ map (\fn -> fn { fnName = makePrelude $ fnName fn }) fns

getRight :: Either e a -> a
getRight (Right x) = x
getRight _ = error "getRight called with a Left"

doInterpret :: Either ProgramError IAST -> IO Value
doInterpret res = if isRight res then interpretAST (getRight res) else return (VAlg "Error occured, exiting!" [])

doChecks :: [String] -> [String] -> String -> Either ProgramError IAST
doChecks fname contents preludeContents = do
  prelude <- mapPrelude <$> parseRealProgram preludeFileName preludeContents
  ast <- zipWithM parseRealProgram fname contents
  let full = foldl1 composeASTs (prelude : ast)
  iast <- first PreprocessingError $ preprocess full
  let typeEnv = mapFromDeclList $ iTypeDecls iast
  traceM $ "Full AST is: " ++ show full
  traceM $ "Full IAST is: " ++ show iast
  first PatternCoverageError $ runCoverageCheck typeEnv $ checkPatterns full
  first TypeError $ typeCheck iast
  return iast

-- prints to stdout to separate from trace output;
-- TODO: replace by die when removing trace
printAndExit :: String -> IO ()
printAndExit s = putStrLn s >> exitFailure

assertFileExists :: String -> IO ()
assertFileExists name = do
  exists <- doesFileExist name
  unless exists . printAndExit $ "Cannot find file \"" ++ name ++ "\"."


main :: IO ()
main = do
  fileNames <- getArgs
  assertFileExists preludeFileName
  mapM_ assertFileExists fileNames
  contents <- mapM readFile fileNames
  preludeContents <- readFile preludeFileName
  let res = doChecks fileNames contents preludeContents
  when (isLeft res) . printAndExit $ showResult res
  v <- doInterpret res
  print v
  hFlush stderr