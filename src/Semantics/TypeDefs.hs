module Semantics.TypeDefs where

import Parser.TypeDefs
import TypeSystem.TypeDefs

import Data.Map (Map)
import Control.Monad.Reader
import Data.List (intercalate)
import Semantics.Builtins (builtinPrefix)
import Control.Monad.State (StateT)

data Value =
    VInt Int
  | VBool Bool
  | VList [Value]
  | VAlg Name [Value]
  | VFun { getFun :: Function }
  | VExpr IExpr
  deriving (Eq)

instance Show Value where
  show (VInt x) = show x
  show (VBool b) = show b
  show (VList l) = "[" ++ intercalate ", " (map show l)  ++ "]"
  show (VAlg name args) = name ++ concatMap ((" " ++) . showValue) args
  show (VFun fn) = show fn
  show (VExpr e) = "thunk{" ++ show e ++ "}"

showValue :: Value -> String
showValue v@(VAlg _ (_:_)) = "(" ++ show v ++ ")"
showValue v = show v

arithErr :: Value
arithErr = error "attempt to use non-int values as ints - this can never happen"

instance Num Value where
  VInt x + VInt y = VInt $ x + y
  _ + _ = arithErr
  VInt x * VInt y = VInt $ x * y
  _ * _ = arithErr
  fromInteger = VInt . fromIntegral
  abs (VInt x) = VInt $ abs x
  abs _ = arithErr
  signum (VInt x) = VInt $ signum x
  signum _ = arithErr
  negate (VInt x) = VInt $ negate x
  negate _ = arithErr

type Env = Map Name Value

data Function =
    Decl { dArgs :: [Name], dBody :: IExpr }
  | Closure { cArg :: Name, cBody :: IExpr, closure :: Env }
  | Builtin { bName :: Name, bArgCount :: Int }
  | Ctor { cName :: Name, cArgCount :: Int }
  | Partial { partialArgs :: [Value], partialFn :: Function}

instance Ord Function where
  _ <= _ = error "cannot compare functions"

instance Eq Function where
  _ == _ = error "cannot compare functions"

instance Show Function where
  show (Decl args body) = "(" ++ unwords args ++ ") => {" ++ show body ++ "}"
  show (Closure arg body _) = arg ++ " => {" ++ show body ++ "}"
  show (Builtin name _) = drop (length builtinPrefix) name
  show (Ctor name _) = "ctor{" ++ name ++ "}"
  show (Partial args fn) = "partial{" ++ show fn ++ " $ " ++ show args ++ "}"


type Eval a = ReaderT Env (ReaderT Env (StateT Int IO)) a