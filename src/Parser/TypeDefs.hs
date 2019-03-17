module Parser.TypeDefs where

import Data.List.NonEmpty

data AST = AST [TypeDecl] [FunDecl] deriving (Show, Eq)

data FunDecl = FunDecl Type Name [Name] Expr deriving (Show, Eq)

data TypeDecl = TypeDecl TypeName [Name] Type deriving (Show, Eq)

data Type = TInt | TBool | TFun [Type] | TAbstract Name deriving (Show, Eq)

type Name = String

type TypeName = String

data Expr = EVar Name | EInt Int | ETypeName TypeName | EAdd Expr Expr | ENeg Expr
  | ESub Expr Expr | EMul Expr Expr | EParen Expr | EApply Expr Expr | ELambda [Name] Expr
  | EList List | EWhere Expr (NonEmpty FunDecl) | EIf BExpr Expr Expr | EMatch Expr [Expr]
  | ELet Name Expr Expr deriving (Show, Eq)

data BExpr = BVal Name | BAnd BExpr BExpr | BOr BExpr BExpr | BNot BExpr | BLeq  Expr Expr deriving (Show, Eq)

data List = LVar Name | LLiteral [Expr] | LCons Expr List | LConcat List List deriving (Show, Eq)
