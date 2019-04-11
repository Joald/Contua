{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as Map
import Data.Map (Map)

-- | If in a monad with a Map-based environment, this function modifies a given key-value pair locally.
localEnv :: (MonadReader (Map a b) m, Ord a) => a -> b -> m c -> m c
localEnv x = local . Map.insert x

-- | Transforms a showable exception into a string exception.
showException :: Show e => Except e a -> Except String a
showException = withExcept show

-- | Generalization of Data.Foldable.concatMap for Data.Traversable.mapM
concatMapM :: (Traversable t, Monad m) => (a -> m [b]) -> t a -> m [b]
concatMapM f xs = concat <$> mapM f xs


pap :: (a -> b, a -> c) -> a -> (b, c)
pap (f, g) x = (f x, g x)