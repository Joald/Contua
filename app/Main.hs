module Main where

import Parser.Parser
import System.Environment
import System.IO
import Control.Monad
import Parser.TypeDefs
import Control.Monad.Trans.Maybe
import TypeSystem.TypeSystem
import TypeSystem.Preprocessor

{- | Prints the contents of all files in the arguments. -}
catMain :: IO ()
catMain = getArgs >>= mapM_ ((>>= print) . readFile)


{- | Parses one file and prints the AST. -}
oneFileParser :: IO ()
oneFileParser = do
  fname:_ <- getArgs
  contents <- readFile fname
  print $ parseProgram fname contents >>= typeCheck . preprocess

main :: IO ()
main = oneFileParser
