module Parser.TypeDefs where

import           Data.List.NonEmpty

data AST = AST [TypeDecl] [FunDecl] deriving (Show, Eq)

data FunDecl = FunDecl { fnType :: Type, fnName :: Name, fnArgs :: [Name], fnBody :: Expr } deriving (Show, Eq)

data TypeVariant = TypeVariant TypeName [Type] deriving (Show, Eq)

-- type declaration contains name, args, and rhs
data TypeDecl = TypeDecl TypeName [Type] [TypeVariant] deriving (Show, Eq)

data Type =
    TCtor TypeName
  | TAbstract Name
  | TList Type
  | TFun Type Type
  | TApply Type Type
  | TPattern
  deriving (Show, Eq)

-- | Type construction helpers.

mkETypeName :: String -> Expr
mkETypeName = ETypeName . TypeName

mkTCtor :: String -> Type
mkTCtor = TCtor . TypeName

aType, intType, aListType, boolType :: Type
intType = mkTCtor "Int"
boolType = mkTCtor "Bool"
aType = TAbstract "a"
aListType = TList aType

binaryType, unaryType :: Type -> Type
binaryType t = t ^->^ t ^->^ t
unaryType t = t ^->^ t


data Pattern =
    Name
  | PTApply TypeName Pattern
  | PCons Pattern Pattern
  | PList Pattern
  deriving (Show, Eq)

infixr 8 ^->^
(^->^) = TFun

infixl 9 ^$$^
(^$$^) = TApply

type Name = String

newtype TypeName = TypeName { unTypeName :: String } deriving (Show, Eq)

data Expr =
    EVar Name
  | EInt Int
  | ETypeName TypeName
  | EAdd Expr Expr
  | ENeg Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EApply Expr Expr
  | ELambda [Name] Expr
  | EListLiteral [Expr]
  | ECons Expr Expr
  | EConcat Expr Expr
  | EIf Expr Expr Expr
  | EMatch Expr [(Expr, Expr)]
  | ELet Name Expr Expr
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

