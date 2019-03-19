module Parser.TypeDefs where

import           Data.List.NonEmpty

data AST = AST [TypeDecl] [FunDecl] deriving (Show, Eq)

data FunDecl = FunDecl Type Name [Expr] Expr deriving (Show, Eq)

data TypeVariant = TypeVariant TypeName [Type] deriving (Show, Eq)

-- type declaration contains name, args, and rhs
data TypeDecl = TypeDecl TypeName [Type] [TypeVariant] deriving (Show, Eq)

data Type =
    TCtor TypeName
  | TAbstract Name
  | TFun Type Type
  | TApply Type Type
  deriving (Show, Eq)

infixr 8 ^->^
infixl 9 ^$$^

(^->^) = TFun
(^$$^) = TApply

type Name = String

type TypeName = String

data Expr =
    EVar Name
  | EInt Int
  | ETypeName TypeName
  | EAdd Expr Expr
  | ENeg Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EApply Expr Expr
  | ELambda [Expr] Expr
  | EListLiteral [Expr]
  | ECons Expr Expr
  | EConcat Expr Expr
  | EIf Expr Expr Expr
  | EMatch Expr [(Expr, Expr)]
  | ELet Expr Expr Expr
  | EAnd Expr Expr
  | EOr Expr Expr
  | ENot Expr
  | EEq  Expr Expr
  | ELeq  Expr Expr
  deriving (Show, Eq)

(^&&^) = EAnd
(^||^) = EOr
(^+^) = EAdd
(^-^) = ESub
(^*^) = EMul
(^$^) = EApply
(^++^) = EConcat
(^:^) = ECons
(^==^) = EEq
(^<=^) = ELeq

