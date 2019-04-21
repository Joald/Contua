module Semantics.TypeDefs where

import Parser.TypeDefs
import TypeSystem.TypeDefs

import Data.Map (Map)
import Control.Monad.Reader

data Value =
    VInt Int
  | VBool Bool
  | VList [Value]
  | VAlg Name [Value]
  | VFun { getFun :: Function }
  deriving (Eq, Show)

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
  deriving (Show)
instance Ord Function where
  _ <= _ = error "cannot compare functions"

instance Eq Function where
  _ == _ = error "cannot compare functions"


type Eval a = Reader Env a