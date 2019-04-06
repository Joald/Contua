{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module TypeSystem.KindChecker where

import           Parser.TypeDefs
import           TypeSystem.Substitutable
import           TypeSystem.TypeDefs

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Utils

type KindCheckT m a = ReaderT KindEnv (StateT KindState (ExceptT KindError m)) a
type KindCheck a = KindCheckT Identity a
newtype KindState = KindState { counter :: Int } deriving (Show, Eq)

initState :: KindState
initState = KindState 0

type KindEnv = Map Name Kind

data KindError =
    OccursCheck Name Kind
  | CannotUnify Kind Kind
  | KindApplicationError Type Kind Type Kind

instance Show KindError where
  show (OccursCheck name k) = "occursCheck: cannot construct infinite kind " ++ name ++ " ~ " ++ show k
  show (CannotUnify k1 k2) = "cannot unify kind " ++ show k1 ++ " with kind " ++ show k2
  show (KindApplicationError t1 k1 t2 k2) =
    "cannot apply type " ++ show t1
    ++ " of kind " ++ show k1
    ++ " to type " ++ show t2
    ++ " of kind " ++ show k2

runKindCheck :: KindEnv -> KindState -> KindCheck a -> Except KindError a
runKindCheck env st m = evalStateT (runReaderT m env) st

freshName :: MonadState KindState m => m Kind
freshName =
  do KindState c <- get
     put $ KindState $ c + 1
     return $ KUnknown $ 'a' : show c

output :: MonadWriter w m => m () -> m w
output m = snd <$> listen m

kindCheckType :: Type -> KindCheck (Kind, KindSubst)
kindCheckType (TApply t1 t2) =
  do name <- freshName
     (k1, s1) <- kindCheckType t1
     (k2, s2) <- local (mapSubstMap s1) $ kindCheckType t2
     s3 <- unifyKinds (apply s2 k1) (KArrow k2 name)
     return (apply s3 name, s3 `compose` s2 `compose` s1)

kindCheckType (TPoly tName) =
  do env <- ask
     case Map.lookup tName env of
       Just kind -> return (kind, nullSubst)
       Nothing   -> (, nullSubst) <$> freshName

kindCheckType (TCtor (TypeName tName)) = kindCheckType $ TPoly tName
kindCheckType (TArrow t1 t2) =
  do (k1, s1) <- kindCheckType t1
     (k2, s2) <- local (mapSubstMap s1) $ kindCheckType t2
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
initArgKinds args = _initArgKinds args <$> (asks (KStar, , ) <*> get)
  where
    _initArgKinds [] es = es
    _initArgKinds (arg:args) es =
      let (k, env, st) = _initArgKinds args es
          (newName, st') = runState freshName st
       in (KArrow newName k, Map.insert arg newName env, st')

kindCheckTypeDecl :: TypeDecl -> KindCheck Kind
kindCheckTypeDecl (TypeDecl (TypeName name) args variants) =
  do env <- ask
     (kind, env', initState) <- initArgKinds (map (\(TPoly x) -> x) args)
     put initState
     subst <- local (const env') $ kindCheckVariants variants
     return $ apply subst kind

kindCheckTypeDecls :: [TypeDecl] -> KindCheck KindEnv
kindCheckTypeDecls [] = ask
kindCheckTypeDecls (decl@(TypeDecl (TypeName name) _ _):decls) = do
  k <- kindCheckTypeDecl decl
  localEnv name k $ kindCheckTypeDecls decls

monomorphise :: Kind -> Kind
monomorphise (KArrow k1 k2) = monomorphise k1 `KArrow` monomorphise k2
monomorphise _              = KStar

monomorphise' :: Map Name Kind -> Map Name Kind
monomorphise' = Map.map monomorphise

doKindCheckIAST :: IAST -> KindCheck KindEnv
doKindCheckIAST (IAST types args) =
  do env <- kindCheckTypeDecls types
     sequence_ $ map (kindCheckType . ifnType) args
     return env

kindCheckIAST :: IAST -> Except String KindEnv
kindCheckIAST ast = showException $ monomorphise' <$> runKindCheck Map.empty initState (doKindCheckIAST ast)

-- | Kind Substitution Solver

type KindSubst = Subst Name Kind

unifyKinds :: Kind -> Kind -> KindCheck KindSubst
unifyKinds (KArrow l1 r1) (KArrow l2 r2) =
  do s1 <- unifyKinds l1 l2
     s2 <- unifyKinds (s1 `apply` r1) (s1 `apply` r2)
     return $ s2 `compose` s1

unifyKinds (KUnknown name) k = bind name k
unifyKinds k (KUnknown name) = bind name k
unifyKinds k1 k2
  | k1 == k2 = return nullSubst
  | otherwise = throwError $ CannotUnify k1 k2

bind :: Name -> Kind -> KindCheck KindSubst
bind name k
  | KUnknown name == k = return nullSubst
  | occursCheck name k = throwError $ OccursCheck name k
  | otherwise = return $ Subst $ Map.singleton name k
