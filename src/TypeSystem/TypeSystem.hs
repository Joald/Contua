{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
module TypeSystem.TypeSystem where

import Parser.TypeDefs
import Data.Map (Map)
import Data.Set ((\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Maybe

import TypeSystem.TypeDefs
import TypeSystem.KindChecker
import TypeSystem.TypeSubstitutable
import Semantics.Builtins
import TypeSystem.BuiltinTypes
import Utils
import Debug.Trace (trace, traceM)

-- | Type system is based on the Hindley-Milner algorithm as presented here:
-- http://dev.stephendiehl.com/fun/006_hindley_milner.html

type SchemeMap = Map Name Scheme

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

typeMapFromDeclList :: [TypeDecl] -> Map Name TypeDecl
typeMapFromDeclList list = Map.fromList $ map (\td -> (tdName td, td)) list

doTypeChecking :: IAST -> TypeCheck IAST
doTypeChecking ast@(IAST types fns) =
  do _ <- liftExcept KindError $ kindCheckIAST ast
     let typeMap = typeMapFromDeclList types
     contFns <- mapM (\fn -> convertTypeToCont (ifnName fn) (ifnType fn) >>= (\t -> return fn {ifnType = t})) fns
     let typeMap'  = Map.map typeFromDecl typeMap
         ctorMap   = Map.fromList $ concatMap (\td -> map (pap (tvName, ForAll (tdArgs td) . foldr (^->^) (typeFromDecl td) . tvArgs)) $ tdVariants td) types
         schemeMap = Map.fromList $ map (\fd -> let t = ifnType fd
                                                  in (ifnName fd, ForAll (Set.toList $ fv t) t)) contFns
     main <- local (const $ TypeEnv (Map.union typeMap' builtinTypes) $ Map.unions [ctorMap, schemeMap, typesOfBuiltins]) $ typeCheckFunctions contFns
     when (isNothing main) $ throwError EntryPointNotFoundError
     traceM $ "Found type " ++ show (fromJust main) ++ " for the main function."
--     unifyTypes intType (fromJust main)
     return ast

genNoEnv :: Type -> Scheme
genNoEnv t = ForAll (Set.toList $ fv t) t

getArgTypes :: Type -> [Name] -> TypeCheck (Type, SchemeMap)
getArgTypes (TArrow t1 t2) (n:ns) =
  do (tBody, rest) <- getArgTypes t2 ns
     return . (tBody,) $ Map.insert n (genNoEnv t1) rest
getArgTypes t [] = return (t, Map.empty)
getArgTypes t _ = throwError $ TooManyArgumentsError t


convertTypeToCont :: Name -> Type -> TypeCheck Type
convertTypeToCont name t
  | isBuiltin name || isPrelude name || tLength t == 0 = return t
  | otherwise = convertTypeToCont' t
    where
      convertTypeToCont' (TArrow t1 t2) = (t1 ^->^) <$> convertTypeToCont' t2
      convertTypeToCont' t' = (\tn -> (t' ^->^ tn) ^->^ tn) <$> freshTypeName

typeCheckFunctions :: [IFnDecl] -> TypeCheck (Maybe Type)
typeCheckFunctions [] = return Nothing
typeCheckFunctions (fn:fns) =
  do t <- typeCheckFunction fn
     mt <- typeCheckFunctions fns
     if | ifnName fn == "main" && isNothing mt -> return $ Just t
        | ifnName fn == "main" -> throwError MultipleEntryPointsFound
        | otherwise -> return mt

isNotArrow :: Type -> Bool
isNotArrow (_ `TArrow` _) = False
isNotArrow _ = True

typeCheckFunction :: IFnDecl -> TypeCheck Type
typeCheckFunction (IFn t name args body) =
  do traceM ("NOW TYPE CHECKING: " ++ name ++ "...")
     (bodyType, argTypes) <- if null args then return (t, Map.empty) else getArgTypes t args
     traceM ("with bodyType " ++ show bodyType ++ " and arg types: " ++ show argTypes)
     (t', s) <- local (mapSchemeEnv $ Map.insert name (genNoEnv t) . Map.union argTypes) $ inferType body
     s' <- unifyTypes (apply s bodyType) t'
     return $ apply s' t'

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

mapSchemeEnv :: (SchemeMap -> SchemeMap) -> TypeEnv -> TypeEnv
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
     return (t2, s2 `compose` s1)

inferType (IEVar x) = trace ("inferType var " ++ x ) $
  do env <- ask
--     traceM $ "looking up variable " ++ x ++ " in env " ++ show (schemeDict env)
     let mt = Map.lookup x $ schemeDict env
         mt' = if isNothing mt
                 then Map.lookup (makePrelude x) $ schemeDict env
                 else mt
     when (isNothing mt') . throwError $ UnboundVariableError x
     t <- instantiateType $ fromJust mt'
     traceM $ "inferred type " ++ show t ++ " for var " ++ x
     return (t, nullSubst)

inferType (ILit (LInt _)) = trace "inferType int literal" $ return (intType, nullSubst)
inferType (ILit LEmptyList) = trace "inferType empty list literal" $ return (aListType, nullSubst)
inferType (IMatch pats x results) =
  do (t1, s1) <- inferType x
     traceM $ "inferred type " ++ show t1 ++ " for matcher " ++ show x
     sm <- mapM (\pat -> localWithSubst s1 $ checkPattern pat t1) pats
     traceM $ "checked patterns " ++ unwords (map show pats) ++ "and got the following identifiers: " ++ unwords (map show sm)
     (t2, s2) <- unzip <$> zipWithM (\sm' res -> local (mapSchemeEnv (Map.union sm')) $ localWithSubst s1 $ inferType res) sm results
     let s3 = foldl1 compose s2
     mapAdjacentM unifyTypes t2
     return (head t2, s3 `compose` s1)

checkPattern :: Pattern -> Type -> TypeCheck SchemeMap
checkPattern (PLit LEmptyList) t = do
  name <- freshTypeName
  _ <- unifyTypes t (TList name)
  return Map.empty
checkPattern (PLit (LInt _)) t = unifyTypes t intType >> return Map.empty
checkPattern (PVar x) t = Map.singleton x <$> generalizeType t
checkPattern (PTVariant name args) t =
  do
    env <- ask
    let ms = Map.lookup name $ schemeDict env
    when (isNothing ms) . throwError $ UnboundVariableError name
    let scheme = fromJust ms
    t' <- instantiateType scheme
    let tArgs = typeArgs t'
    s <- unifyTypes t (typeBody t')
    mconcat <$> zipWithM checkPattern args (map (apply s) tArgs)

checkPattern (PCons x xs) t =
  do t' <- freshTypeName
     let tl = TList t'
     s1 <- unifyTypes tl t
     sm1 <- checkPattern x t'
     sm2 <- checkPattern xs (apply s1 tl)
     return $ sm1 `Map.union` sm2