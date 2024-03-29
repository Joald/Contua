module Parser.TypeDefs where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

data AST = AST { typeDecls :: [TypeDecl], typeAliases :: [TypeAlias], funDecls :: [FunDecl] } deriving (Eq)

instance Show AST where
  show (AST ts aliases fns) =
    intercalate "\n\n" $ map show ts ++ map show aliases ++ map show fns

data FunDecl = FunDecl { fnContType :: Maybe Type, fnType :: Maybe Type, fnName :: Name, fnArgs :: [Name], fnBody :: Expr } deriving (Eq)

data TypeAlias = Alias { aliasName :: Name, aliasType :: Type } deriving (Show, Eq)
unAlias :: TypeAlias -> (Name, Type)
unAlias (Alias name t) = (name, t)

mapFromDeclList :: [TypeDecl] -> Map Name TypeDecl
mapFromDeclList list = Map.fromList $ map (\td -> (tdName td, td)) list

instance Show FunDecl where
  show (FunDecl contType t name args body) =
      maybe "" ((++ " :\n") . show) contType
    ++ maybe "" ((++ " ::\n") . show) t
    ++ name
    ++ concatMap (" "++) args
    ++ " = "
    ++ show body

data TypeVariant = TypeVariant { tvName :: Name, tvArgs :: [Type] } deriving (Eq)

instance Show TypeVariant where
  show (TypeVariant name args) = unwords $ name : map show args

data TypeDecl = TypeDecl { tdName :: Name, _tdArgs :: [Type], tdVariants :: [TypeVariant] } deriving (Eq)

tdArgs :: TypeDecl -> [Name]
tdArgs = map (\(TVar t) -> t) . _tdArgs

typeFromDecl :: TypeDecl -> Type
typeFromDecl (TypeDecl name args _) = foldl (^$$^) (TName name) args

instance Show TypeDecl where
  show (TypeDecl name args variants) =
      "type "
    ++ name
    ++ (if null args then "" else " ")
    ++ unwords (map show args)
    ++ " = "
    ++ intercalate " | " (map show variants)

data Kind =
    KStar
  | KArrow Kind Kind
  | KUnknown Name
  deriving (Eq, Ord)

showKind :: Kind -> String
showKind t@(KArrow _ _) = '(' : show t ++ ")"
showKind t = show t

instance Show Kind where
  show KStar = "*"
  show (KArrow k1 k2) = showKind k1 ++ " -> " ++ show k2
  show (KUnknown name) = name

data Type =
    TName Name
  | TVar Name
  | TList Type
  | TArrow Type Type
  | TApply Type Type
  | TBottom
  | TBuiltin { unBuiltin :: Name }
  | TCont (Maybe Type) Type
  | TNotFunction Type
  deriving (Eq, Ord)

typeArgs :: Type -> [Type]
typeArgs (TArrow t1 t2) = t1 : typeArgs t2
typeArgs _ = []

typeBody :: Type -> Type
typeBody (TArrow _ t) = typeBody t
typeBody t = t

tLength :: Type -> Int
tLength (TArrow _ t) = 1 + tLength t
tLength _ = 0

instance Show Type where
--  show (TName name) = "name{" ++ name ++ "}"
  show (TName name) = "" ++ name ++ ""
--  show (TVar name) = "var{" ++ name ++ "}"
  show (TVar name) = "" ++ name ++ ""
  show (TList t) = "[" ++ show t ++ "]"
  show (TArrow t1 t2) = showType t1 ++ " -> " ++ show t2
  show (TApply t1 t2) = show t1 ++ " " ++ show t2
  show TBottom = "⊥"
  show (TBuiltin name) = "" ++ name ++ ""
  show (TCont tc t) = show t ++ " with continuation " ++ show tc
  show (TNotFunction t) = "non-fn{" ++ show t ++ "}"

showType :: Type -> String
showType t@(TArrow _ _) = "(" ++ show t ++ ")"
showType t = show t

-- | Type construction helpers.

aType, intType, aListType, boolType :: Type
intType = TName "Int"
boolType = TName "Bool"
aType = TVar "a"
aListType = TList aType

binaryType, unaryType :: Type -> Type
binaryType t = t ^->^ t ^->^ t
unaryType t = t ^->^ t

(^->^), (^$$^) :: Type -> Type -> Type

infixr 8 ^->^
(^->^) = TArrow

infixl 9 ^$$^
(^$$^) = TApply

type Name = String

data Expr =
    EVar Name
  | EInt Int
  | ETypeName Name
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
  | ELes  Expr Expr
  | EGre  Expr Expr
  | EGeq  Expr Expr
  deriving (Show, Eq)

gatherArgs :: Expr -> [Expr]
gatherArgs (EApply e1 e2) = gatherArgs e1 ++ [e2]
gatherArgs _ = []

leftmost :: Expr -> Expr
leftmost (EApply e _) = leftmost e
leftmost x = x


(^&&^), (^||^), (^+^), (^-^), (^*^), (^$^), (^++^), (^:^), (^==^), (^<=^):: Expr -> Expr -> Expr
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

