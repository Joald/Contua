{-# LANGUAGE FlexibleInstances #-}

module TypeSystem.KindSubstitutable where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Parser.TypeDefs

newtype KindSubst = Subst { unSubst :: Map Name Kind } deriving (Eq, Show)

instance Semigroup KindSubst where
  (<>) = compose

instance Monoid KindSubst where
  mempty = Subst Map.empty

nullSubst :: KindSubst
nullSubst = mempty

singletonSubst :: Name -> Kind -> KindSubst
singletonSubst name = Subst . Map.singleton name

substAdd :: Name -> Kind -> KindSubst -> KindSubst
substAdd k v (Subst m) = Subst $ Map.update (const $ Just v) k m

compose :: KindSubst -> KindSubst -> KindSubst
s1'@(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply s1') s2 `Map.union` s1

occursCheck ::  KindSubstitutable a => Name -> a -> Bool
occursCheck a t = a `Set.member` fv t

class KindSubstitutable a where
  fv :: a -> Set Name
  apply :: KindSubst -> a -> a


instance KindSubstitutable Kind where
  fv KStar = Set.empty
  fv (KUnknown x) = Set.singleton x
  fv (KArrow k1 k2) = Set.union (fv k1) (fv k2)
  apply _ KStar = KStar
  apply subst k@(KUnknown x) = Map.findWithDefault k x (unSubst subst)
  apply subst (KArrow k1 k2) = KArrow (apply subst k1) (apply subst k2)

instance KindSubstitutable a => KindSubstitutable [a] where
  fv = foldr (Set.union . fv) Set.empty
  apply = map . apply

instance KindSubstitutable a => KindSubstitutable (Map Name a) where
  fv = fv . Map.elems
  apply = Map.map . apply

mapSubstMap :: (Map Name Kind -> Map Name Kind) -> KindSubst -> KindSubst
mapSubstMap f = Subst . f . unSubst
