module Parser.TypeDefs where

import           Data.List.NonEmpty

data AST = AST [TypeDecl] [FunDecl] deriving (Show, Eq)

data FunDecl = FunDecl { fnType :: Type, fnName :: Name, fnArgs :: [Name], fnBody :: Expr } deriving (Show, Eq)

data TypeVariant = TypeVariant { tvName :: TypeName, tvArgs :: [Type] } deriving (Show, Eq)

-- type declaration contains name, args, and rhs
data TypeDecl = TypeDecl TypeName [Type] [TypeVariant] deriving (Show, Eq)

data Kind =
    KStar
  | KArrow Kind Kind
  | KUnknown Name
  deriving (Eq)

showKind :: Kind -> String
showKind t@(KArrow _ _) = '(' : show t ++ ")"
showKind t = show t

instance Show Kind where
  show KStar = "*"
  show (KArrow k1 k2) = showKind k1 ++ " -> " ++ show k2
  show (KUnknown name) = name

data Type =
    TCtor TypeName
  | TPoly Name
  | TList Type
  | TArrow Type Type
  | TApply Type Type
  | TPattern Type
  deriving (Eq)

instance Show Type where
  show (TPoly name) = name
  show (TList t) = "[" ++ show t ++ "]"
  show (TApply t1 t2) = show t1 ++ " " ++ show t2
  show (TCtor (TypeName name)) = name
  show (TArrow t1 t2) = showType t1 ++ " -> " ++ show t2

showType :: Type -> String
showType t@(TArrow _ _) = "(" ++ show t ++ ")"
showType t = show t

-- | Type construction helpers.

mkETypeName :: String -> Expr
mkETypeName = ETypeName . TypeName

mkTCtor :: String -> Type
mkTCtor = TCtor . TypeName

aType, intType, aListType, boolType :: Type
intType = mkTCtor "Int"
boolType = mkTCtor "Bool"
aType = TPoly "a"
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
(^->^) = TArrow

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

