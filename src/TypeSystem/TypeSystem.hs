{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
module TypeSystem.TypeSystem where

import Parser.TypeDefs
import Data.Map (Map)
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import Data.Maybe
import Data.Bifunctor

import TypeSystem.TypeDefs
import TypeSystem.KindChecker
import TypeSystem.TypeSubstitutable
import Semantics.Builtins
import Utils
import Debug.Trace (trace, traceM)

-- | Type system is based on the Hindley-Milner algorithm as presented here:
-- http://dev.stephendiehl.com/fun/006_hindley_milner.html

typeCheck :: IAST -> Either TypeSystemError IAST
typeCheck ast = runTypeCheck (doTypeChecking ast)

defaultEnv :: TypeEnv
defaultEnv = TypeEnv Map.empty Map.empty

runTypeCheck :: TypeCheck a -> Either TypeSystemError a
runTypeCheck tc = runExcept $ evalStateT (runReaderT tc defaultEnv) $ IState 0

liftExcept :: (b -> TypeSystemError) -> Except b a -> TypeCheck a
liftExcept err = lift . lift . withExcept err

freshTypeName :: TypeCheck Type
freshTypeName =
  do IState n <- get
     put . IState $ n + 1
     return . TVar $ "a" ++ show n

doTypeChecking :: IAST -> TypeCheck IAST
doTypeChecking ast@(IAST types fns) =
  do kindEnv <- liftExcept KindError $ kindCheckIAST ast
     let typeMap = Map.fromList $ map (\td -> let name = tdName td
                                                in (name, (td, Map.lookup name kindEnv))) types
     when (any (isNothing . snd) $ Map.elems typeMap) .
       throwError $ KindError "Some type declaration(s) were not kind checked."
     fns <- mapM (\fn -> convertTypeToCont (ifnName fn) (ifnType fn) >>= (\t -> return fn {ifnType = t})) fns
     let typeMap'  = Map.map (typeFromDecl . fst) typeMap
         ctorMap   = Map.fromList $ concatMap (\td -> map (pap (tvName, ForAll (tdArgs td) . foldr (^->^) (typeFromDecl td) . tvArgs)) $ tdVariants td) types
         schemeMap = Map.fromList $ map (\fd -> let t = ifnType fd
                                                  in (ifnName fd, ForAll (Set.toList $ fv t) t)) fns
     local (const $ TypeEnv (Map.union typeMap' builtinTypes) $ Map.unions [ctorMap, schemeMap, typesOfBuiltins]) $ typeCheckFunctions fns
     return ast

genNoEnv :: Type -> Scheme
genNoEnv t = ForAll (Set.toList $ fv t) t

getArgTypes :: Type -> [Name] -> TypeCheck (Type, Map Name Scheme)
getArgTypes (TArrow t1 t2) (n:ns) =
  do (tBody, rest) <- getArgTypes t2 ns
     return . (tBody,) $ Map.insert n (genNoEnv t1) rest
getArgTypes t [] = return (t, Map.empty)
getArgTypes t _ = throwError $ TooManyArgumentsError t


convertTypeToCont :: Name -> Type -> TypeCheck Type
convertTypeToCont name t
  | isBuiltin name = return t
  | otherwise = convertTypeToCont' t
    where
      convertTypeToCont' (TArrow t1 t2) = (t1 ^->^) <$> convertTypeToCont' t2
      convertTypeToCont' t = (\tn -> (t ^->^ tn) ^->^ tn) <$> freshTypeName

typeCheckFunctions :: [IFnDecl] -> TypeCheck ()
typeCheckFunctions [] = return ()
typeCheckFunctions (fn:fns) =
  do typeCheckFunction fn
     typeCheckFunctions fns

isNotArrow :: Type -> Bool
isNotArrow (_ `TArrow` _) = False
isNotArrow _ = True

typeCheckFunction :: IFnDecl -> TypeCheck ()
typeCheckFunction (IFn t name args body) =
  do traceM ("NOW TYPE CHECKING: " ++ name ++ "...")
     (bodyType, argTypes) <- if null args then return (t, Map.empty) else getArgTypes t args
     traceM ("with bodyType " ++ show bodyType ++ " and arg types: " ++ show argTypes)
     (t', s) <- local (mapSchemeEnv $ Map.insert name (genNoEnv t) . Map.union argTypes) $ inferType body
     void (unifyTypes bodyType t')

unifyTypes :: Type -> Type -> TypeCheck TypeSubst
unifyTypes t1@(TArrow l1 r1) t2@(TArrow l2 r2) = trace ("unify types " ++ show t1 ++ " with " ++ show t2) $
  do s1 <- unifyTypes l1 l2
     s2 <- unifyTypes (apply s1 r1) (apply s1 r2)
     return $ s2 `compose` s1

unifyTypes t1@(TApply l1 r1) t2@(TApply l2 r2) = trace ("unify types " ++ show t1 ++ " with " ++ show t2) $
  do s1 <- unifyTypes l1 l2
     s2 <- unifyTypes (apply s1 r1) (apply s1 r2)
     return $ s2 `compose` s1

unifyTypes (TList t1) (TList t2) = trace ("unify types " ++ show (TList t1) ++ " with " ++ show (TList t2)) $ unifyTypes t1 t2
unifyTypes (TVar n) t = trace ("unify type variable " ++ n ++ " with type " ++ show t) $ bindType n t
unifyTypes t (TVar n) = trace ("unify type variable " ++ n ++ " with type " ++ show t) $ bindType n t
unifyTypes t1@(TBuiltin n1) t2@(TBuiltin n2)
  | n1 == n2  = return nullSubst
  | otherwise = throwError $ UnificationError t1 t2
unifyTypes t1@(TBuiltin n1) t2@(TName n2)
  | n1 == n2 = return nullSubst
  | otherwise = throwError $ UnificationError t1 t2
unifyTypes t1@(TName n1) t2@(TBuiltin n2)
  | n1 == n2 = return nullSubst
  | otherwise = throwError $ UnificationError t1 t2
unifyTypes (TName n) t = trace ("lookup type name " ++ n ++ " and unify with type " ++ show t) $ lookupType n t
unifyTypes t (TName n) = trace ("lookup type name " ++ n ++ " and unify with type " ++ show t) $ lookupType n t
unifyTypes TBottom _ = return nullSubst
unifyTypes _ TBottom = return nullSubst
unifyTypes t1 t2 = trace ("cannot unify " ++ show t1 ++ " with " ++ show t2) $ throwError $ UnificationError t1 t2

bindType :: Name -> Type -> TypeCheck TypeSubst
bindType n t
  | TVar n == t = return nullSubst
  | occursCheck n t = throwError $ OccursCheck n t
  | otherwise = return . Subst $ Map.singleton n t


lookupType :: Name -> Type -> TypeCheck TypeSubst
lookupType n t
  | TName n == t = return nullSubst
  | otherwise =
  do env <- ask
     let m = Map.lookup n $ typeDict env
     when (isNothing m) . throwError $ UnboundTypeVariableError n t
     traceM $ "Looked up " ++ n ++ " and found " ++ show (fromJust m)
     unifyTypes t $ fromJust m

tr :: Show a => a -> a
tr x = trace (show x) x

localWithSubst :: TypeSubst -> TypeCheck a -> TypeCheck a
localWithSubst s = local (\env -> env { schemeDict = apply s $ schemeDict env })

instantiateType :: Scheme -> TypeCheck Type
instantiateType (ForAll vars t) =
  flip apply t . Subst . Map.fromList <$> mapM (\var -> (var,) <$> freshTypeName) vars

generalizeType :: Type -> TypeCheck Scheme
generalizeType t = asks $ \env -> ForAll (Set.toList $ fv t \\ fv (schemeDict env)) t

mapSchemeEnv :: (Map Name Scheme -> Map Name Scheme) -> TypeEnv -> TypeEnv
mapSchemeEnv f env = env { schemeDict = f $ schemeDict env }

inferType :: IExpr -> TypeCheck (Type, TypeSubst)
inferType (IEApply e1 e2) = trace ("inferType (" ++ show e1 ++ ") (" ++ show e2 ++ ")\n") $
  do (t1, s1) <- inferType e1
--     traceM $ "inference for the function returned " ++ show (t1, s1)
     (t2, s2) <- localWithSubst s1 $ inferType e2
--     traceM $ "inference for the argument returned " ++ show (t2, s2)
     name     <- freshTypeName
     s3       <- unifyTypes (apply s2 t1) (t2 ^->^ name)
--     traceM $ "s1: " ++ show s1
--     traceM $ "s2: " ++ show s2
--     traceM $ "s3: " ++ show s3
     traceM $ "inferred type " ++ show (apply s3 name) ++ " for (" ++ show e1 ++ ") (" ++ show e2 ++ ") "
     return   (apply s3 name, s3 `compose` s2 `compose` s1)

inferType (IEAbstract v e) = trace ("inferType λ" ++ v ++ " . " ++ show e ++ "\n") $
  do name <- freshTypeName
     let nameScheme = ForAll [] name
     (t1, s1) <- local (mapSchemeEnv $ Map.insert v nameScheme) $ inferType e
     traceM $ "inferred type " ++ show (apply s1 name ^->^ t1) ++ " for λ" ++ v ++ " . " ++ show e ++ ""
     return (apply s1 name ^->^ t1, s1)

inferType (IELet x e1 e2) = trace ("inferType let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ "\n") $
  do (t1, s1) <- inferType e1
     traceM $ "inference for " ++ show e1 ++ " generated subst " ++ show s1
     t1' <- localWithSubst s1 $ generalizeType t1
--     traceM $ "generalized let binding of " ++ show t1' ++ " to " ++ x
     (t2, s2) <- localWithSubst s1 $ local (mapSchemeEnv $ Map.insert x t1') $ inferType e2
     traceM $ "let body returned " ++ show (t2, s2)
--     traceM $ "whole let will yield substitution " ++ show (s2 `compose` s1)
     traceM $ "inferred type " ++ show t2 ++ " for let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ""
     return (t2, s2 `compose` s1) -- TODO: think about composition order

inferType (IEVar x) = trace ("inferType var " ++ x ) $
  do env <- ask
--     traceM $ "looking up variable " ++ x ++ " in env " ++ show (schemeDict env)
     let mt = Map.lookup x $ schemeDict env
     when (isNothing mt) . throwError $ UnboundVariableError x
     t <- instantiateType $ fromJust mt
     traceM $ "inferred type " ++ show t ++ " for var " ++ x
     return (t, nullSubst)

inferType (ILit (LInt _)) = trace "inferType int literal" $ return (intType, nullSubst)
inferType (ILit LEmptyList) = trace "inferType empty list literal" $ return (aListType, nullSubst)
inferType (ILit LError) = trace "inferType error literal" $ return (TBottom, nullSubst)
-- TODO: pattern :))))