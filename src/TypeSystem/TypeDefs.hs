{-# LANGUAGE MultiWayIf #-}
module TypeSystem.TypeDefs where

import Parser.TypeDefs
import Control.Monad.Except (Except)
import Data.Map (Map)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.List (intercalate)
import Semantics.Builtins
import Control.Monad.Writer (WriterT)

-- | The I prefix stands for Internal (or intermediate :P)

data IAST = IAST { iTypeDecls :: [ITypeDecl], iFnDecls :: [IFnDecl] } deriving (Eq)

instance Show IAST where
  show (IAST tDecls fnDecls) = intercalate "\n\n" $ map show tDecls ++ map show fnDecls

type ITypeDecl = TypeDecl

data IFnDecl = IFnDecl { ifnContType :: Maybe Type, ifnType :: Maybe Type, ifnName :: Name, ifnArgs :: [Name], ifnBody :: IExpr } deriving (Eq)

instance Show IFnDecl where
  show (IFnDecl contType t name args body) =
    maybe "" ((++ " :\n") . show) contType
    ++ maybe "" ((++ " ::\n") . show) t
    ++ name ++ (if null args then "" else " ") ++ unwords args ++ " = " ++ show body

data IExpr =
    IEAbstract Name IExpr
  | IEApply IExpr IExpr
  | IELet Name IExpr IExpr
  | IEVar Name
  | IMatch [Pattern] IExpr [IExpr]
  | ILit Lit
  | IIf IExpr IExpr IExpr
  deriving (Eq)

data Pattern =
    PVar Name
  | PTVariant Name [Pattern]
  | PCons Pattern Pattern
  | PLit Lit
  deriving (Eq)

instance Show Pattern where
  show (PVar name) = name
  show (PTVariant name pats) = name ++ " " ++ unwords (map showPattern pats)
  show (PCons h t) = show h ++ ":" ++ show t
  show (PLit l) = show l

showPattern :: Pattern -> String
showPattern (PVar name) = name
showPattern (PLit l) = show l
showPattern t = "(" ++ show t ++ ")"

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
    ++ " with "
    ++ unwords (zipWith (\pat res ->
      " | " ++ show pat ++ " => " ++ show res) pats results)
  show (IIf b e1 e2) = "if " ++ show b ++ " then " ++ show e1 ++ " else " ++ show e2

data Lit =
    LInt Int
  | LEmptyList
  deriving (Eq)

instance Show Lit where
  show (LInt x) = show x
  show LEmptyList = "[]"

type Context = String

data TypeSystemError =
    KindError String
  | UnboundTypeVariableError Name
  | UnboundVariableError Name
  | UnificationError Type Type
  | OccursCheck Name Type
  | TooManyArgumentsError Type
  | MultipleBindings Name Context
  | EntryPointNotFoundError
  | ForbiddenRecursiveConstant Name
  | NonContinuation Type
  | FunctionComparison Type

instance Show TypeSystemError where
  show (KindError s) = "Kind error occured: " ++ s
  show (UnboundTypeVariableError n) = "Unbound type variable " ++ n ++ "."
  show (UnboundVariableError n) = "Unbound variable: " ++ show n
  show (UnificationError t1 t2) = "Cannot unify type " ++ show t1 ++ " with " ++ show t2
  show (OccursCheck n t) = "Occurs check: cannot construct infinite type " ++ n ++ " ~ " ++ show t
  show (TooManyArgumentsError t) = "Type " ++ show t ++ " is not a function type."
  show (MultipleBindings name context) = "Multiple bindings of identifier " ++ name ++ " in " ++ context
  show EntryPointNotFoundError = "Cannot find the entry point of the program. Did you specify the `main` function?"
  show (ForbiddenRecursiveConstant name) = "Forbidden recursive use of constant" ++ name
  show (NonContinuation t) = "Forbidden non-continuation style type of top-level function " ++ show t
  show (FunctionComparison t) = "Cannot compare objects of type " ++ show t
type TypeCheck a = WriterT (Map Name Type) (ReaderT TypeEnv (StateT InferenceState (ReaderT String (Except TypeSystemError)))) a

newtype InferenceState = IState { counter :: Int }

data TypeEnv = TypeEnv { typeDict :: Map Name TypeDecl, schemeDict :: Map Name Scheme } deriving (Show, Eq)

data Scheme = ForAll { schVars :: [Name], schT :: Type } deriving (Eq)

instance Show Scheme where
  show (ForAll vars t) = (if null vars then "" else "∀" ++ unwords vars ++ " . ") ++ show t