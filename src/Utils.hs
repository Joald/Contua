{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as Map
import Data.Map (Map)

-- | If in a monad with a Map-based environment, this function modifies a given key-value pair locally.
localEnv :: (MonadReader (Map a b) m, Ord a) => a -> b -> m c -> m c
localEnv x y = local $ Map.alter (const $ Just y) x

-- | Transforms a showable exception into a string exception.
showException :: Show e => Except e a -> Except String a
showException = withExcept show
