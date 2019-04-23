{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module TypeSystem.KindChecker where

import           Parser.TypeDefs
import           TypeSystem.KindSubstitutable
import           TypeSystem.TypeDefs

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Utils
import Data.Maybe (catMaybes)
import Debug.Trace (traceM)

type KindCheckT m a = ReaderT KindEnv (StateT KindState (ExceptT KindError m)) a
type KindCheck a = KindCheckT Identity a
newtype KindState = KindState { counter :: Int } deriving (Show, Eq)

initState :: KindState
initState = KindState 0

type KindEnv = Map Name Kind

data KindError =
    KindOccursCheck Name Kind
  | CannotUnify Kind Kind

instance Show KindError where
  show (KindOccursCheck name k) = "Occurs check: cannot construct infinite kind " ++ name ++ " ~ " ++ show k
  show (CannotUnify k1 k2) = "Cannot unify kind " ++ show k1 ++ " with kind " ++ show k2

runKindCheck :: KindEnv -> KindState -> KindCheck a -> Except KindError a
runKindCheck env st m = evalStateT (runReaderT m env) st

freshKindName :: MonadState KindState m => m Kind
freshKindName =
  do KindState c <- get
     put $ KindState $ c + 1
     return $ KUnknown $ 'a' : show c


kindCheckType :: Type -> KindCheck (Kind, KindSubst)
kindCheckType (TApply t1 t2) =
  do name <- freshKindName
     (k1, s1) <- kindCheckType t1
     (k2, s2) <- local (apply s1) $ kindCheckType t2
     s3 <- unifyKinds (apply s2 k1) (KArrow k2 name)
     return (apply s3 name, s3 `compose` s2 `compose` s1)

kindCheckType (TVar tName) =
  do env <- ask
     case Map.lookup tName env of
       Just kind -> return (kind, nullSubst)
       Nothing   -> (, nullSubst) <$> freshKindName

kindCheckType (TName tName) = kindCheckType $ TVar tName
kindCheckType (TArrow t1 t2) =
  do (_, s1) <- kindCheckType t1
     (_, s2) <- local (apply s1) $ kindCheckType t2
     return (KStar, s2 `compose` s1)

kindCheckType _ = return (KStar, nullSubst)

kindCheckVariantArgs :: [Type] -> KindCheck KindSubst
kindCheckVariantArgs = foldM (\a t -> (a <>) . snd <$> kindCheckType t) nullSubst

kindCheckVariants :: [TypeVariant] -> KindCheck KindSubst
kindCheckVariants [] = return nullSubst
kindCheckVariants (TypeVariant _ args:tvs) =
  do subst  <- kindCheckVariantArgs args
     subst' <- kindCheckVariants tvs
     return $ subst' `compose` subst

initArgKinds :: [Name] -> KindCheck (Kind, KindEnv, KindState)
initArgKinds _args = _initArgKinds _args <$> (asks (KStar, , ) <*> get)
  where
    _initArgKinds [] es = es
    _initArgKinds (arg:args) es =
      let (k, env, st) = _initArgKinds args es
          (newName, st') = runState freshKindName st
       in (KArrow newName k, Map.insert arg newName env, st')

kindCheckTypeDecl :: TypeDecl -> KindCheck Kind
kindCheckTypeDecl (TypeDecl _ args variants) =
  do (kind, env', newState) <- initArgKinds (map (\(TVar x) -> x) args)
     put newState
     subst <- local (const env') $ kindCheckVariants variants
     return $ apply subst kind

kindCheckTypeDecls :: [TypeDecl] -> KindCheck KindEnv
kindCheckTypeDecls [] = ask
kindCheckTypeDecls (decl@(TypeDecl name _ _):decls) = do
  k <- kindCheckTypeDecl decl
  localEnv name k $ kindCheckTypeDecls decls

monomorphise :: Kind -> Kind
monomorphise (KArrow k1 k2) = monomorphise k1 `KArrow` monomorphise k2
monomorphise _              = KStar

monomorphise' :: Map Name Kind -> Map Name Kind
monomorphise' = Map.map monomorphise

doKindCheckIAST :: IAST -> KindCheck KindEnv
doKindCheckIAST (IAST types fns) =
  do env <- monomorphise' <$> kindCheckTypeDecls types
     traceM $ "\n\nKind check of IAST:\n" ++ showList' types ++ "\n" ++ showList' fns
     traceM $ "THIS IS THE ENV:\n" ++ showMap env
     x <- local (const env) $ mapM kindCheckType $ catMaybes $ map ifnType fns ++ map ifnContType fns
     traceM $ "THIS IS THE FINAL KINDCHECK:\n" ++ showMap (Map.fromList x)
     s <- local (const env) $ foldAdjacentM unifyKinds compose nullSubst $ map fst x ++ [KStar]
     traceM $ "THIS IS AFTER UNIFICATION:\n" ++ showMap (unSubst s)
     return env

kindCheckIAST :: IAST -> Except String KindEnv
kindCheckIAST ast = showException $ runKindCheck Map.empty initState (doKindCheckIAST ast)

-- | Kind Substitution Solver

unifyKinds :: Kind -> Kind -> KindCheck KindSubst
unifyKinds (KArrow l1 r1) (KArrow l2 r2) =
  do s1 <- unifyKinds l1 l2
     s2 <- unifyKinds (s1 `apply` r1) (s1 `apply` r2)
     return $ s2 `compose` s1

unifyKinds (KUnknown name) k = bindKind name k
unifyKinds k (KUnknown name) = bindKind name k
unifyKinds k1 k2
  | k1 == k2 = return nullSubst
  | otherwise = throwError $ CannotUnify k1 k2

bindKind :: Name -> Kind -> KindCheck KindSubst
bindKind name k
  | KUnknown name == k = return nullSubst
  | occursCheck name k = throwError $ KindOccursCheck name k
  | otherwise = return $ Subst $ Map.singleton name k
