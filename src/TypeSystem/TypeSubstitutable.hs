{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module TypeSystem.TypeSubstitutable where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set


import Parser.TypeDefs
import TypeSystem.TypeDefs
import Debug.Trace (trace)


newtype TypeSubst = Subst { unSubst :: Map Name Type } deriving (Eq, Show)

nullSubst :: TypeSubst
nullSubst = Subst Map.empty

singletonSubst :: Name -> Type -> TypeSubst
singletonSubst name = Subst . Map.singleton name

substAdd :: Name -> Type -> TypeSubst -> TypeSubst
substAdd k v (Subst m) = Subst $ Map.update (const $ Just v) k m

compose :: TypeSubst -> TypeSubst -> TypeSubst
s1'@(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply s1') s2 `Map.union` s1

occursCheck ::  TypeSubstitutable a => Name -> a -> Bool
occursCheck a t = a `Set.member` fv t

class TypeSubstitutable a where
  fv :: a -> Set Name
  apply :: TypeSubst -> a -> a

instance TypeSubstitutable a => TypeSubstitutable [a] where
  fv = foldr (Set.union . fv) Set.empty
  apply = map . apply

instance TypeSubstitutable a => TypeSubstitutable (Map Name a) where
  fv = fv . Map.elems
  apply = Map.map . apply

mapSubstMap :: (Map Name Type -> Map Name Type) -> TypeSubst -> TypeSubst
mapSubstMap f = Subst . f . unSubst

instance TypeSubstitutable Scheme where
  fv (ForAll vars t) = fv t \\ Set.fromList vars
  apply subst (ForAll vars t) = ForAll vars $ apply (mapSubstMap (flip (foldr Map.delete) vars) subst) t

instance TypeSubstitutable Type where
  fv (TApply t1 t2) = Set.union (fv t1) (fv t2)
  fv (TArrow t1 t2) = Set.union (fv t1) (fv t2)
  fv (TList t)      = fv t
  fv (TNotFunction t)= fv t
  fv (TVar n)       = Set.singleton n
  fv (TName _)      = Set.empty
  fv (TBuiltin _)   = Set.empty
  fv t              = trace ("PALISIEKURWA " ++ show t) Set.empty

  apply subst (TArrow t1 t2) = TArrow (apply subst t1) (apply subst t2)
  apply subst (TApply t1 t2) = TApply (apply subst t1) (apply subst t2)
  apply subst (TList t) = TList $ apply subst t
  apply subst t@(TVar n) = Map.findWithDefault t n (unSubst subst)
  apply subst (TNotFunction t) = TNotFunction $ apply subst t
  apply _ t = t

instance TypeSubstitutable TypeDecl where
  fv = undefined
  apply subst td@(TypeDecl { tdVariants, ..}) = td { tdVariants = apply subst tdVariants }


instance TypeSubstitutable TypeVariant where
  fv = undefined
  apply subst (TypeVariant name args) = TypeVariant name $ apply subst args
