{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeSystem.TypeSystem (typeCheck) where

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
import Control.Monad.Writer
import Data.Bifunctor (second)
import qualified Data.List as L
import Control.Applicative (liftA2)
import Data.List (nub)

-- | Type system is based on the Hindley-Milner algorithm as presented here:
-- http://dev.stephendiehl.com/fun/006_hindley_milner.html

type SchemeMap = Map Name Scheme

typeCheck :: IAST -> Either TypeSystemError ()
typeCheck ast = runTypeCheck (doTypeChecking ast)

defaultEnv :: TypeEnv
defaultEnv = TypeEnv Map.empty Map.empty

runTypeCheck :: TypeCheck a -> Either TypeSystemError a
runTypeCheck tc = second fst $ runExcept (evalStateT (runReaderT (runWriterT tc) defaultEnv) $ IState 0)

liftExcept :: (b -> TypeSystemError) -> Except b a -> TypeCheck a
liftExcept err = lift . lift . lift . withExcept err

throwWhenM :: MonadError a m => m Bool -> a -> m ()
throwWhenM b e = whenM b $ throwError e

throwWhenMultipleEqual :: [Name] -> String -> TypeCheck ()
throwWhenMultipleEqual xs context = when (length (nub xs) /= length xs) . throwError $ MultipleBindings (show . head $ xs L.\\ nub xs) context

throwWhenNothing :: MonadError e m => Maybe a -> e -> m ()
throwWhenNothing ma err = when (isNothing ma) $ throwError err

freshTypeName :: TypeCheck Type
freshTypeName = do
  IState n <- get
  put . IState $ n + 1
  return . TVar $ "a" ++ show n

typesOfBuiltins :: TypeCheck (Map BuiltinName Scheme)
typesOfBuiltins = Map.fromList . zip builtinNames . map generalizeBuiltin <$> mapM preprocessType builtinsTypes

preprocessType :: Type -> TypeCheck Type
preprocessType t = do
  let fvt = Set.toList $ fv t
  names <- Subst . Map.fromList <$> forM fvt (\v -> (v, ) <$> freshTypeName)
  return $ apply names t

preprocessFns :: [IFnDecl] -> TypeCheck [IFnDecl]
preprocessFns = mapM $ _mapType >=> _mapContType
  where
    _mapType fn     = maybe (return fn) (fmap (\t' -> fn { ifnType     = Just t' }) . preprocessType) $ ifnType fn
    _mapContType fn = maybe (return fn) (fmap (\ct -> fn { ifnContType = Just ct }) . preprocessType) $ ifnContType fn

declToScheme :: ITypeDecl -> [TypeVariant] -> [Scheme]
declToScheme td = map $ ForAll (tdArgs td) . foldr (^->^) (typeFromDecl td) . tvArgs

preprocessTypeDecl :: ITypeDecl -> TypeCheck ITypeDecl
preprocessTypeDecl (TypeDecl name args variants) = do
  args' <- mapM preprocessType args
  let argNames = map (\(TVar n) -> n) args
      subst = Subst . Map.fromList $ zip argNames args'
      variants' = map (preprocessVariant subst) variants
  return $ TypeDecl name args' variants'
    where preprocessVariant s (TypeVariant name' args') = TypeVariant name' $ apply s args'


getCtorTypes :: [ITypeDecl] -> TypeCheck SchemeMap
getCtorTypes types = do
  types' <- mapM preprocessTypeDecl types
  return $ Map.fromList $ concatMap (\td@(TypeDecl _ _ variants) ->
                        zip (map tvName variants) $ declToScheme td variants
                      ) types'

doTypeChecking :: IAST -> TypeCheck ()
doTypeChecking ast@(IAST types fns) = do
  -- Check for name clashes.
  let globalIdentifiers = map ifnName fns ++ concatMap (map tvName . tdVariants) types
  throwWhenMultipleEqual globalIdentifiers "program"
  let globalTypes = map tdName types ++ Map.keys builtinTypes
  throwWhenMultipleEqual globalTypes "program"
  -- Perform kind checking.
  void(liftExcept KindError $ kindCheckIAST ast)
  ctorMap <- getCtorTypes types
  let  -- Apply continuation modification to functions with type annotations
    contFns  = map (\fn -> fn { ifnType = case ifnContType fn of
      Nothing -> convertTypeToCont (ifnName fn) <$> ifnType fn <*> pure (TVar "")
      Just ct -> convertTypeToCont (ifnName fn) <$> ifnType fn <*> pure ct
    }) fns
    typeMap = mapFromDeclList types

  finalFns <- preprocessFns contFns
  schemeMap <- Map.fromList <$>
       forM finalFns (\t -> do
         name <- freshTypeName
         let t' = fromMaybe name (ifnType t)
         s <- generalizeType t'
         return (ifnName t, s))
  typesOfBuiltins' <- typesOfBuiltins
  ((main, s), m) <- listen $ local (const $ TypeEnv (typeMap <> builtinTypeDecls) $ Map.unions [ctorMap, schemeMap, typesOfBuiltins']) $ typeCheckFunctions finalFns
  throwWhenNothing main EntryPointNotFoundError
  finalTypes <- mapMapM generalizeType $ fix' (apply s) m
  -- verify once more to account for non-annotated functions
  void $ local (const $ TypeEnv (typeMap <> builtinTypeDecls) $ Map.unions [ctorMap, finalTypes, typesOfBuiltins']) $ typeCheckFunctions finalFns


convertTypeToCont :: Name -> Type -> Type -> Type
convertTypeToCont name t contType
  | isBuiltin name || isPrelude name || tLength t == 0 = t
  | otherwise = convertTypeToCont' t contType
    where
      convertTypeToCont' (TArrow t1 t2) ct = t1 ^->^ convertTypeToCont' t2 ct
      convertTypeToCont' t' ct = (t' ^->^ ct) ^->^ ct

typeCheckFunctions :: [IFnDecl] -> TypeCheck (Maybe Type, TypeSubst)
typeCheckFunctions [] = return (Nothing, nullSubst)
typeCheckFunctions (fn:fns) = do
  (t, s1) <- typeCheckFunction fn
  s <- generalizeType t
  (mt, s2) <- local (mapSchemeEnv $ Map.insert (ifnName fn) s) . localWithSubst s1 $ typeCheckFunctions fns
  return $ if ifnName fn == "main"
            then (Just t, s2 `compose` s1)
            else (mt, s2 `compose` s1)


typeCheckFunction :: IFnDecl -> TypeCheck (Type, TypeSubst)
typeCheckFunction (IFnDecl _ (Just t) name args body) = do
  (funType, s) <- doTypeCheckFunction args body
  s' <- unifyTypes funType t
  let s'' = s' `compose` s
  tell $ Map.singleton name $ apply s'' funType
  return (apply s'' funType, s'')
typeCheckFunction (IFnDecl _ Nothing name args body) = do
  (funType, s) <- doTypeCheckFunction args body
  s' <- if isBuiltin name || isPrelude name
        then return nullSubst
        else verifyContinuativity funType
  tell $ Map.singleton name funType
  return (funType, s' `compose` s)

verifyContinuativity :: Type -> TypeCheck TypeSubst
verifyContinuativity t = case typeArgs t of
  [] -> return nullSubst
  [_] -> throwError $ NonContinuation t
  args -> unifyTypes (typeBody t) $ typeBody $ last args

doTypeCheckFunction :: [Name] -> IExpr -> TypeCheck (Type, TypeSubst)
doTypeCheckFunction args body =
  do argTypes <- mapM (\arg -> (arg, ) . ForAll [] <$> freshTypeName) args
     let argMap = Map.fromList argTypes
     (t, s) <- local (mapSchemeEnv $ Map.union argMap) $ inferType body
     return (foldr ((^->^) . apply s . schT . snd) (apply s t) argTypes, s)

containsFn :: Type -> TypeCheck Bool
containsFn (TArrow _ _) = return True
containsFn (TApply t1 t2) = liftA2 (&&) (containsFn t1) (containsFn t2)
containsFn (TList t) = containsFn t
containsFn (TNotFunction t) = containsFn t
containsFn (TName name) = do
  env <- ask
  let mt = Map.lookup name $ typeDict env
  throwWhenNothing mt $ UnboundTypeVariableError name
  let TypeDecl _ _ variants = fromJust mt
  fmap or $ mapM containsFn $ concatMap tvArgs variants
containsFn _ = return False

unifyTypes :: Type -> Type -> TypeCheck TypeSubst
unifyTypes (TArrow l1 r1) (TArrow l2 r2) = unifyTypes (TApply l1 r1) (TApply l2 r2)
unifyTypes (TApply l1 r1) (TApply l2 r2) = do
  s1 <- unifyTypes l1 l2
  s2 <- unifyTypes (apply s1 r1) (apply s1 r2)
  return $ s2 `compose` s1

unifyTypes (TList t1) (TList t2) = unifyTypes t1 t2

unifyTypes (TVar n) t = bindType n t
unifyTypes t (TVar n) = bindType n t

unifyTypes (TNotFunction t1) (TNotFunction t2) = do
  throwWhenM (containsFn t1) $ FunctionComparison t1
  throwWhenM (containsFn t2) $ FunctionComparison t2
  unifyTypes t1 t2

unifyTypes (TNotFunction (TVar a)) t = do
  throwWhenM (containsFn t) $ FunctionComparison t
  unifyTypes (TVar a) t

unifyTypes t t'@(TNotFunction (TVar _)) = unifyTypes t' t

unifyTypes (TNotFunction t1) t = do
  throwWhenM (containsFn t) $ FunctionComparison t
  throwWhenM (containsFn t1) $ FunctionComparison t1
  unifyTypes t1 t
unifyTypes t t'@(TNotFunction _) = unifyTypes t' t

unifyTypes t1@(TBuiltin n1) t2@(TBuiltin n2)
  | n1 == n2  = return nullSubst
  | otherwise = throwError $ UnificationError t1 t2
unifyTypes t1@(TBuiltin n1) t2@(TName n2)
  | n1 == n2 = return nullSubst
  | otherwise = throwError $ UnificationError t1 t2
unifyTypes t1@(TName n1) t2@(TBuiltin n2)
  | n1 == n2 = return nullSubst
  | otherwise = throwError $ UnificationError t1 t2

unifyTypes (TName n1) (TName n2) | n1 == n2 = return nullSubst
unifyTypes t1 t2 = throwError $ UnificationError t1 t2

bindType :: Name -> Type -> TypeCheck TypeSubst
bindType n t
  | TVar n == t = return nullSubst
  | occursCheck n t = throwError $ OccursCheck n t
  | otherwise = return . Subst $ Map.singleton n t

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
inferType (IEApply e1 e2) = do
  (t1, s1) <- inferType e1
  (t2, s2) <- localWithSubst s1 $ inferType e2
  name     <- freshTypeName
  s3       <- unifyTypes (apply s2 t1) (t2 ^->^ name)
  return   (apply s3 name, s3 `compose` s2 `compose` s1)

inferType (IEAbstract v e) = do
  name <- freshTypeName
  let nameScheme = ForAll [] name
  (t1, s1) <- local (mapSchemeEnv $ Map.insert v nameScheme) $ inferType e
  return (apply s1 name ^->^ t1, s1)

inferType (IELet x e1 e2) = do
  (t1, s1) <- inferType e1
  t1' <- localWithSubst s1 $ generalizeType t1
  (t2, s2) <- localWithSubst s1 $ local (mapSchemeEnv $ Map.insert x t1') $ inferType e2
  return (t2, s2 `compose` s1)

inferType (IEVar x) = do
  env <- ask
  let mt = Map.lookup x $ schemeDict env
      mt' = if isNothing mt
             then Map.lookup (makePrelude x) $ schemeDict env
             else mt
  throwWhenNothing mt' $ UnboundVariableError x
  t <- instantiateType $ fromJust mt'
  return (t, nullSubst)

inferType (ILit (LInt _)) = return (intType, nullSubst)
inferType (ILit LEmptyList) = (, nullSubst) <$> preprocessType aListType
inferType (IMatch pats x results) = do
  (t1, s1) <- inferType x
  (sm, _s2) <- mapAndUnzipM (\pat -> localWithSubst s1 $ checkPattern pat t1) pats
  let s2 = foldr compose nullSubst _s2
  (t2, s3) <- inferPatterns sm results
  let finalSubst = s3 `compose` s2 `compose` s1
  return (apply finalSubst t2, finalSubst)

inferType (IIf b e1 e2) = do
  (t1, s1) <- inferType b
  s2 <- unifyTypes t1 boolType
  (t2, s3) <- localWithSubst (s2 `compose` s1) $ inferType e1
  (t3, s4) <- localWithSubst (s3 `compose` s2 `compose` s1) $ inferType e2
  s5 <- unifyTypes (apply s4 t2) t3
  return (apply s5 t3, s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1)



inferPatterns :: [SchemeMap] -> [IExpr] -> TypeCheck (Type, TypeSubst)
inferPatterns [sc] [e] = local (mapSchemeEnv (sc <>)) $ inferType e
inferPatterns (sc:scs) (e:es) = do
  (t1, s1) <- inferPatterns [sc] [e]
  (t2, s2) <- localWithSubst s1 $ inferPatterns scs es
  s3 <- unifyTypes (apply s2 t1) t2
  return (apply s3 t2, s3 `compose` s2 `compose` s1)
inferPatterns _ _ = error "invalid pattern list: this cannot happen"

checkPattern :: Pattern -> Type -> TypeCheck (SchemeMap, TypeSubst)
checkPattern (PLit LEmptyList) t = do
  name <- freshTypeName
  s <- unifyTypes t (TList name)
  return (Map.empty, s)

checkPattern (PLit (LInt _)) t = (Map.empty, ) <$> unifyTypes t intType
checkPattern (PVar x) t | x /= "_" = return (Map.singleton x $ ForAll [] t, nullSubst)
checkPattern (PVar _) _ = return (Map.empty, nullSubst)
checkPattern p@(PTVariant name args) t = do
  env <- ask
  let ms = Map.lookup name $ schemeDict env
  throwWhenNothing ms $ UnboundVariableError name
  let scheme = fromJust ms
  t' <- instantiateType scheme
  let tArgs = typeArgs t'
  s <- unifyTypes t (typeBody t')
  (maps, substs) <- unzip <$> zipWithM checkPattern args (map (apply s) tArgs)
  let names = concatMap Map.keys maps
  throwWhenMultipleEqual names (show p)
  return (mconcat maps, foldl compose s substs)

checkPattern (PCons x xs) t = do
  t' <- freshTypeName
  let tl = TList t'
  s1 <- unifyTypes tl t
  (sm1, s2) <- checkPattern x (apply s1 t')
  let s2' = s2 `compose` s1
  (sm2, s3) <- checkPattern xs (apply s2' tl)
  return (sm1 `Map.union` sm2, s3 `compose` s2')
