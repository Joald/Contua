module TypeSystem.TypeSystem where

import Parser.TypeDefs
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import TypeSystem.TypeDefs
import TypeSystem.KindChecker

-- | Type system is based on the Hindley-Milner algorithm
-- as presented here: http://dev.stephendiehl.com/fun/006_hindley_milner.html

typeCheck :: IAST -> Either TypeError IAST
typeCheck ast = runTypeCheck (doTypeChecking ast)

runTypeCheck :: TypeCheck a -> Either TypeError a
runTypeCheck tc = runExcept $ evalStateT (runReaderT tc $ TypeEnv Map.empty) $ IState 0

liftExcept :: (b -> TypeError) -> Except b a -> TypeCheck a
liftExcept err = lift . lift . withExcept err

doTypeChecking :: IAST -> TypeCheck IAST
doTypeChecking ast =
  do kindEnv <- liftExcept KindError $ kindCheckIAST ast
     return ast

