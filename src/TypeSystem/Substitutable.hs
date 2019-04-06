{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module TypeSystem.Substitutable where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Parser.TypeDefs

data Subst a b where
  Subst :: (Substitutable a b, Ord a) => Map a b -> Subst a b

unSubst :: (Ord a) => Subst a b -> Map a b
unSubst (Subst m) = m

deriving instance (Show a, Show b) => Show (Subst a b)
deriving instance (Eq a, Eq b) => Eq (Subst a b)

instance (Ord a, Ord b) => Ord (Subst a b) where
  s1 `compare` s2 = unSubst s1 `compare` unSubst s2

instance (Substitutable a b) => Semigroup (Subst a b) where
  (<>) = compose

instance (Substitutable a b) => Monoid (Subst a b) where
  mempty = Subst Map.empty

nullSubst :: Substitutable a b => Subst a b
nullSubst = mempty

substAdd :: Ord a => a -> b -> Subst a b -> Subst a b
substAdd k v (Subst m) = Subst $ Map.update (const $ Just v) k m

compose :: (Substitutable a b) => Subst a b -> Subst a b -> Subst a b
s1'@(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply s1') s2 `Map.union` s1

occursCheck ::  Substitutable a b => a -> b -> Bool
occursCheck a t = a `Set.member` fv t

class (Ord var) => Substitutable var a | a -> var where
  fv :: a -> Set var
  apply :: Subst var a -> a -> a


instance Substitutable Name Kind where
  fv KStar = Set.empty
  fv (KUnknown x) = Set.singleton x
  fv (KArrow k1 k2) = Set.union (fv k1) (fv k2)
  apply _ KStar = KStar
  apply subst k@(KUnknown x) = Map.findWithDefault k x (unSubst subst)
  apply subst (KArrow k1 k2) = KArrow (apply subst k1) (apply subst k2)

mapSubstMap :: Substitutable var a => Subst var a -> Map var a -> Map var a
mapSubstMap = Map.map . apply

mapSubstList :: Substitutable var a => Subst var a -> [a] -> [a]
mapSubstList = map . apply