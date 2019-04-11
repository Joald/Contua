{-# LANGUAGE MultiWayIf #-}
module TypeSystem.TypeDefs where

import Parser.TypeDefs
import Control.Monad.RWS (RWST)
import Control.Monad.Except (Except)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.List (intercalate)

-- | The I prefix stands for Internal (or intermediate :P)

data IAST = IAST [ITDecl] [IFnDecl] deriving (Eq)

instance Show IAST where
  show (IAST tDecls fnDecls) = intercalate "\n" $ map show tDecls ++ map show fnDecls

type ITDecl = TypeDecl

data IFnDecl = IFn { ifnType :: Type, ifnName :: Name, ifnArgs :: [Name], ifnBody :: IExpr } deriving (Eq)

instance Show IFnDecl where
  show (IFn t name args body) = show t ++ "::\n" ++ name ++ " " ++ unwords args ++ " = " ++ show body

data IExpr =
    IEAbstract Name IExpr
  | IEApply IExpr IExpr
  | IELet Name IExpr IExpr
  | IEVar Name
  | IPat IExpr
  | ILit Lit
  deriving (Eq)

showIExpr :: IExpr -> String
showIExpr expr =
  let p = "(" ++ show expr ++ ")"
   in if | IEAbstract _ _ <- expr -> p
         | IELet {} <- expr -> p
         | otherwise -> show expr

showIExpr' :: IExpr -> String
showIExpr' expr =
  let p = "(" ++ show expr ++ ")"
   in if | IEAbstract _ _ <- expr -> p
         | IELet {} <- expr -> p
         | IEApply _ _ <- expr -> p
         | otherwise -> show expr

instance Show IExpr where
  show (IEAbstract name e) = "λ" ++ name ++ " . " ++ show e
  show (IEApply e1 e2) = showIExpr e1 ++ " " ++ showIExpr' e2
  show (IELet x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2
  show (IEVar x) = x
  show (IPat e) = "pat{" ++ show e ++ "}"
  show (ILit lit) = show lit


data Lit =
    LInt Int
  | LEmptyList
  | LError
  deriving (Eq)

instance Show Lit where
  show (LInt x) = show x
  show LEmptyList = "[]"
  show LError = "Error"

data TypeSystemError =
    KindError String
  | UnboundTypeVariableError Name Type
  | UnboundVariableError Name
  | UnificationError Type Type
  | OccursCheck Name Type
  | TooManyArgumentsError Type

instance Show TypeSystemError where
  show (KindError s) = "Kind error occured: " ++ s
  show (UnboundTypeVariableError n t) = "Unbound type variable " ++ n ++ ", expected " ++ show t
  show (UnboundVariableError n) = "Unbound variable: " ++ show n
  show (UnificationError t1 t2) = "Cannot unify type " ++ show t1 ++ " with " ++ show t2
  show (OccursCheck n t) = "Occurs check: cannot construct infinite type " ++ n ++ " ~ " ++ show t
  show (TooManyArgumentsError t) = "Type " ++ show t ++ " is not a function type."

type TypeCheck a = ReaderT TypeEnv (StateT InferenceState (Except TypeSystemError)) a

newtype InferenceState = IState { counter :: Int }

data TypeEnv = TypeEnv { typeDict :: Map Name Type, schemeDict :: Map Name Scheme } deriving (Show, Eq)

data Scheme = ForAll { vars :: [Name], t :: Type} deriving (Eq)

instance Show Scheme where
  show (ForAll vars t) = (if null vars then "" else "∀" ++ unwords vars ++ " . ") ++ show t