{-# LANGUAGE FlexibleContexts #-}
module TypeSystem.Aliasing where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative (liftA2)
import Data.List (intersect)

import Parser.TypeDefs
import Utils

type AliasMap = Map Name Type

applyAlias :: MonadReader AliasMap m => Type -> m Type
applyAlias t
  | TName n <- t = asks $ Map.findWithDefault t n
  | TList t' <- t = TList <$> applyAlias t'
  | TArrow t1 t2 <- t = liftA2 TArrow (applyAlias t1) (applyAlias t2)
  | TApply t1 t2 <- t = liftA2 TApply (applyAlias t1) (applyAlias t2)
  | TCont t1 t2  <- t = liftA2 TCont (mapM applyAlias t1) (applyAlias t2)
  | otherwise = return t

preprocessAliases :: [TypeAlias] -> Either String [TypeAlias]
preprocessAliases aliases = runExcept $ evalStateT (runReaderT (doAliasing aliases) . Map.fromList $ map unAlias aliases) Set.empty

type Aliaser = ReaderT AliasMap (StateT (Set Name) (Except String))

applyAliasToAlias :: TypeAlias -> Aliaser TypeAlias
applyAliasToAlias (Alias name t) = Alias name <$> applyAlias t

doAliasing :: [TypeAlias] -> Aliaser [TypeAlias]
doAliasing aliases = mapM_ go aliases >> mapM (fixM applyAliasToAlias) aliases
  where
    go alias = do
      put Set.empty
      checkAlias alias

-- | Checking if aliases form a cycle.

data GraphNode a = Node a [GraphNode a]

names :: Type -> [Name]
names (TName n) = [n]
names (TList t) = names t
names (TArrow t1 t2) = names t1 ++ names t2
names (TApply t1 t2) = names t1 ++ names t2
names _ = []

register :: Name -> Aliaser ()
register name = modify $ Set.insert name

throwIfSet :: Name -> Aliaser ()
throwIfSet name = do
  s <- get
  when (Set.member name s) $ throwError $ "Loop in aliases: check usage of alias " ++ name ++ "."

checkAlias :: TypeAlias -> Aliaser ()
checkAlias (Alias name t) = do
  throwIfSet name
  register name
  env <- ask
  let subAliases = names t `intersect` Map.keys env
  mapM_ checkAlias $ zipWith Alias subAliases $ map (env !) subAliases
