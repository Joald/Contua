{-# LANGUAGE MultiWayIf #-}
module TypeSystem.TypeDefs where

import Parser.TypeDefs
import Control.Monad.Except (Except)
import Data.Map (Map)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.List (intercalate)
import Semantics.Builtins
import Data.Maybe (isJust, fromJust)

-- | The I prefix stands for Internal (or intermediate :P)

data IAST = IAST [ITDecl] [IFnDecl] deriving (Eq)

instance Show IAST where
  show (IAST tDecls fnDecls) = intercalate "\n" $ map show tDecls ++ map show fnDecls

type ITDecl = TypeDecl

data IFnDecl = IFn { ifnContType :: Maybe Type, ifnType :: Maybe Type, ifnName :: Name, ifnArgs :: [Name], ifnBody :: IExpr } deriving (Eq)

instance Show IFnDecl where
  show (IFn contType t name args body) =
    (if isJust contType
       then show (fromJust contType) ++ " :\n"
       else "") ++ (if isJust t
                     then show (fromJust t) ++ " ::\n"
                     else "") ++ name ++ " " ++ unwords args ++ " = " ++ show body

data IExpr =
    IEAbstract Name IExpr
  | IEApply IExpr IExpr
  | IELet Name IExpr IExpr
  | IEVar Name
  | IMatch [Pattern] IExpr [IExpr]
  | ILit Lit
  deriving (Eq)

data Pattern =
    PVar Name
  | PTVariant Name [Pattern]
  | PCons Pattern Pattern
  | PLit Lit
  deriving (Show, Eq)

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
  show (IEVar x)
    | isBuiltin x = drop (length builtinPrefix) x
    | isPrelude x = drop (length preludePrefix) x
    | otherwise   = x
  show (ILit lit) = show lit
  show (IMatch pats x results) =
      "match "
    ++ show x
    ++ " with\n"
    ++ intercalate "\n" (zipWith (\pat res ->
      "  | " ++ show pat ++ " => " ++ show res) pats results)


data Lit =
    LInt Int
  | LEmptyList
  deriving (Eq)

instance Show Lit where
  show (LInt x) = show x
  show LEmptyList = "[]"

data TypeSystemError =
    KindError String
  | UnboundTypeVariableError Name Type
  | UnboundVariableError Name
  | UnificationError Type Type
  | OccursCheck Name Type
  | TooManyArgumentsError Type
  | EntryPointNotFoundError
  | MultipleEntryPointsFound

instance Show TypeSystemError where
  show (KindError s) = "Kind error occured: " ++ s
  show (UnboundTypeVariableError n t) = "Unbound type variable " ++ n ++ ", expected " ++ show t
  show (UnboundVariableError n) = "Unbound variable: " ++ show n
  show (UnificationError t1 t2) = "Cannot unify type " ++ show t1 ++ " with " ++ show t2
  show (OccursCheck n t) = "Occurs check: cannot construct infinite type " ++ n ++ " ~ " ++ show t
  show (TooManyArgumentsError t) = "Type " ++ show t ++ " is not a function type."
  show EntryPointNotFoundError = "Cannot find the entry point of the program. Did you specify the `main` function?"
  show MultipleEntryPointsFound = "Multiple main functions found."

type TypeCheck a = ReaderT TypeEnv (StateT InferenceState (Except TypeSystemError)) a

newtype InferenceState = IState { counter :: Int }

data TypeEnv = TypeEnv { typeDict :: Map Name Type, schemeDict :: Map Name Scheme } deriving (Show, Eq)

data Scheme = ForAll { schVars :: [Name], schT :: Type } deriving (Eq)

instance Show Scheme where
  show (ForAll vars t) = (if null vars then "" else "∀" ++ unwords vars ++ " . ") ++ show t