module Main where

import Parser.Parser
import System.Environment
import System.IO
import Control.Monad
import Parser.TypeDefs
import Control.Monad.Trans.Maybe


parseFile :: String -> Maybe AST
parseFile fname = do

  return $ AST [] []


{- | Prints the contents of all files in the arguments. -}
catMain :: IO ()
catMain = getArgs >>= mapM_ ((>>= print) . readFile)


{- | Parses one file and prints the AST. -}
oneFileParser :: IO ()
oneFileParser = do
  x <- getArgs
  let fname = head x
  contents <- readFile fname
  print $ parseProgram fname contents

main :: IO ()
main = oneFileParser
