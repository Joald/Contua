module TypeSystem.TypeDefs where

import Parser.TypeDefs

-- | The I prefix stands for Internal (or intermediate :P)

data IAST = IAST [ITDecl] [IFnDecl] deriving (Show, Eq)

type ITDecl = TypeDecl

data IFnDecl = IFn Type Name [Name] IExpr deriving (Show, Eq)

data IExpr =
    IEAbstract Name IExpr
  | IEApply IExpr IExpr
  | IELet Name IExpr IExpr
  | IEVar Name
  | IETypeCtor TypeName
  | ILit Lit
  deriving (Show, Eq)

data Lit =
    LInt Int
  | LEmptyList
  | LError
  deriving (Show, Eq)