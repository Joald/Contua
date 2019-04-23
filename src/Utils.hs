{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (intercalate)

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

-- | Applies a function f on all consecutive pairs of elements of the list, then discards the result.
mapAdjacentM :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAdjacentM _ [] = return ()
mapAdjacentM _ [_] = return ()
mapAdjacentM f (x1:x2:xs) = f x1 x2 >> mapAdjacentM f (x2:xs)


-- | Applies a function f on all consecutive pairs of elements of the list and accumulates the result.
--   It is a left fold.
foldAdjacentM :: Monad m => (a -> a -> m b) -> (b -> b -> b) -> b -> [a] -> m b
foldAdjacentM _ _ acc []          = return acc
foldAdjacentM _ _ acc [_]        = return acc
foldAdjacentM f g acc (x1:x2:xs) = do
  res1 <- f x1 x2
  foldAdjacentM f g (g acc res1) (x2:xs)


-- | Generalization of Map.map to monadic actions.
--   Basically a fusion of Map.map and mapM
mapMapM :: (Monad m, Ord a) => (b -> m c) -> Map a b -> m (Map a c)
mapMapM f m = Map.fromList . zip (Map.keys m) <$> mapM f (Map.elems m)

forMapM :: (Monad m, Ord a) => Map a b -> (b -> m c) -> m (Map a c)
forMapM = flip mapMapM

mapMapWithKeyM :: (Monad m, Ord a) => (a -> b -> m c) -> Map a b -> m (Map a c)
mapMapWithKeyM f m = Map.fromList . zip (Map.keys m) <$> zipWithM f (Map.keys m) (Map.elems m)

forMapWithKeyM :: (Monad m, Ord a) => Map a b -> (a -> b -> m c) -> m (Map a c)
forMapWithKeyM = flip mapMapWithKeyM

-- | utility shows for better printing
showMap :: (Show a, Show b) => Map a b -> String
showMap m = replicate 20 '-' ++ concatMap (("\n" ++) . showPair) (Map.toList m) ++ "\n" ++ replicate 20 '-'

showPair :: (Show a, Show b) => (a, b) -> String
showPair (x, y) = show x ++ " := " ++ show y

showList' :: Show a => [a] -> String
showList' = intercalate "\n" . map show


-- | Given a function that is an endomorphism and an argument,
-- returns the fixed point by repeatedly applying the function to the result.
fix' :: Eq a => (a -> a) -> a -> a
fix' f x =
  let y = f x
    in if x == y
         then y
         else fix' f y;
