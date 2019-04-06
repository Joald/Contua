module TypeSystem.TypeDefs where

import Parser.TypeDefs
import Control.Monad.RWS (RWST)
import Control.Monad.Except (Except)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)

-- | The I prefix stands for Internal (or intermediate :P)

data IAST = IAST [ITDecl] [IFnDecl] deriving (Show, Eq)

type ITDecl = TypeDecl

data IFnDecl = IFn { ifnType :: Type, ifnName :: Name, ifnArgs :: [Name], ifnBody :: IExpr } deriving (Show, Eq)

data IExpr =
    IEAbstract Name IExpr
  | IEApply IExpr IExpr
  | IELet Name IExpr IExpr
  | IEVar Name
  | ILit Lit
  deriving (Show, Eq)

data Lit =
    LInt Int
  | LEmptyList
  | LError
  deriving (Show, Eq)

data TypeError = KindError String

type TypeCheck a = ReaderT TypeEnv (StateT InferenceState (Except TypeError)) a

newtype InferenceState = IState { counter :: Int }

newtype TypeEnv = TypeEnv { unTypeEnv :: Map Name (Type, Kind) }