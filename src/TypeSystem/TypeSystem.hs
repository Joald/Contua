{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Debug.Trace (trace)
import qualified Debug.Trace (traceM)
import Control.Monad.Writer
import Data.Bifunctor (second, first)
import Data.List (intercalate, nub)
import qualified Data.List as L

liftPrefix :: ReaderT String (Except TypeSystemError) a -> TypeCheck a
liftPrefix = lift . lift . lift

traceM :: String -> TypeCheck ()
traceM s = do
  prefix <- liftPrefix ask
  Debug.Trace.traceM $ prefix ++ s

addPrefix :: MonadReader String m => String -> m a -> m a
addPrefix s m = local (++ s) m

-- | Type system is based on the Hindley-Milner algorithm as presented here:
-- http://dev.stephendiehl.com/fun/006_hindley_milner.html

-- TODO: error when multiple args with same name

type SchemeMap = Map Name Scheme

typeCheck :: IAST -> Either TypeSystemError ()
typeCheck ast = runTypeCheck (doTypeChecking ast)

defaultEnv :: TypeEnv
defaultEnv = TypeEnv Map.empty Map.empty

runTypeCheck :: TypeCheck a -> Either TypeSystemError a
runTypeCheck tc = second fst $ runExcept (runReaderT (evalStateT (runReaderT (runWriterT tc) defaultEnv) $ IState 0) "")

liftExcept :: (b -> TypeSystemError) -> Except b a -> TypeCheck a
liftExcept err = lift . lift . lift . lift . withExcept err

freshTypeName :: TypeCheck Type
freshTypeName =
  do IState n <- get
     put . IState $ n + 1
     traceM $ "getting fresh type " ++ show n ++ "!"
     return . TVar $ "a" ++ show n

mapFromDeclList :: [TypeDecl] -> Map Name TypeDecl
mapFromDeclList list = Map.fromList $ map (\td -> (tdName td, td)) list

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
    _mapType fn     = maybe (return fn) (fmap (\t' -> fn { ifnType = Just t' }) . preprocessType) $ ifnType fn
    _mapContType fn = maybe (return fn) (fmap (\ct' -> fn { ifnContType = Just ct' }) . preprocessType) $ ifnContType fn

declToScheme :: ITypeDecl -> [TypeVariant] -> {-TypeCheck-} [Scheme]
declToScheme td = map $ ForAll (tdArgs td) . foldr (^->^) (typeFromDecl td) . tvArgs

typeVar :: Type -> Name
typeVar (TVar n) = n
typeVar _ = error "type argument is not abstract: this should never happen"

preprocessTypeDecl :: ITypeDecl -> TypeCheck ITypeDecl
preprocessTypeDecl (TypeDecl name args variants) = do
  args' <- mapM preprocessType args
  let argNames = map typeVar args
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
doTypeChecking ast@(IAST types fns) =
  do _ <- liftExcept KindError $ kindCheckIAST ast
     ctorMap <- getCtorTypes types
     let  -- Apply continuation modification to functions with type annotations
        contFns  = map (\fn -> fn { ifnType = case ifnContType fn of
          Nothing -> convertTypeToCont (ifnName fn) <$> ifnType fn <*> pure (TVar "")
          Just ct -> convertTypeToCont (ifnName fn) <$> ifnType fn <*> pure ct
        }) fns
        typeMap  = Map.map typeFromDecl $ mapFromDeclList types

     finalFns <- preprocessFns contFns
     traceM $ "finalFns: " ++ showList' finalFns ++ "\n"
     schemeMap <- Map.fromList <$>
           forM finalFns (\t -> do
             name <- freshTypeName
             let t' = fromMaybe name (ifnType t)
             s <- generalizeType t'
             return (ifnName t, s))
     typesOfBuiltins' <- typesOfBuiltins
     traceM $ "types of builtins: " ++ showMap typesOfBuiltins' ++ "\n"
     ((main, s), m) <- listen $ local (const $ TypeEnv (Map.union typeMap builtinTypes) $ Map.unions [ctorMap, schemeMap, typesOfBuiltins']) $ typeCheckFunctions finalFns
     when (isNothing main) $ throwError EntryPointNotFoundError
     traceM $ "Found type " ++ show (fromJust main) ++ " for the main function."
     traceM $ "Final subst is:\n" ++ showMap (unSubst s)
     traceM $ "\n\nfinal types are:\n" ++ showMap (fix' (apply s) m)

convertTypeToCont :: Name -> Type -> Type -> Type
convertTypeToCont name t contType
  | isBuiltin name || isPrelude name || tLength t == 0 = t
  | otherwise = convertTypeToCont' t contType
    where
      convertTypeToCont' (TArrow t1 t2) ct = t1 ^->^ convertTypeToCont' t2 ct
      convertTypeToCont' t' ct = (t' ^->^ ct) ^->^ ct

typeCheckFunctions :: [IFnDecl] -> TypeCheck (Maybe Type, TypeSubst)
typeCheckFunctions [] = return (Nothing, nullSubst)
typeCheckFunctions (fn:fns) =
  do (t, s1) <- typeCheckFunction fn
     (mt, s2) <- localWithSubst s1 $ typeCheckFunctions fns
     if | ifnName fn == "main" && isNothing mt -> return (Just t, s2 `compose` s1)
        | ifnName fn == "main" -> throwError MultipleEntryPointsFound
        | otherwise -> return (mt, s2 `compose` s1)

isNotArrow :: Type -> Bool
isNotArrow (_ `TArrow` _) = False
isNotArrow _ = True

addArgs :: [Name] -> Map Name Type -> Type -> TypeCheck Type
addArgs (n:ns) m t =
  do let mArgType = Map.lookup n m
     when (isNothing mArgType) . throwError $ TooManyArgumentsError t
     t' <- addArgs ns m t
     let argType = fromJust mArgType
     return $ argType ^->^ t'
addArgs _ _ t = return t

getArgTypes :: Type -> [Name] -> TypeCheck (Type, SchemeMap)
getArgTypes (TArrow t1 t2) (n:ns) =
  do (tBody, rest) <- getArgTypes t2 ns
     return (tBody, Map.insert n (ForAll [] t1) rest)
getArgTypes t [] = return (t, Map.empty)
getArgTypes t _ = throwError $ TooManyArgumentsError t

typeCheckFunction :: IFnDecl -> TypeCheck (Type, TypeSubst)
typeCheckFunction fd@(IFnDecl _ (Just t) name args _) =
  do traceM ("\nNOW TYPE CHECKING: " ++ name ++ "...")
     (bodyType, argTypes) <- if null args then return (t, Map.empty) else getArgTypes t args
     traceM ("with bodyType " ++ show bodyType ++ " and arg types: " ++ show argTypes)
     (t', s) <- typeCheckFunction fd { ifnType = Nothing }--local (mapSchemeEnv $ Map.insert name (genNoEnv t) . Map.union argTypes) $ inferType body
     traceM $ "\nFinished inferring: "
     at <- mapMapM instantiateType argTypes
     s' <- unifyTypes (apply s bodyType) t'
     traceM $ "Found type " ++ show (apply s' t')
     finalType <- addArgs args (apply s' at) (apply s' t')
     tell $ Map.singleton name finalType
     return (apply s' t', s')
typeCheckFunction (IFnDecl _ Nothing _name args body) =
  do traceM ("NOW INFERRING: " ++ _name ++ "...")
     argTypes <- Map.fromList <$> mapM (\arg -> (arg, ) . ForAll [] <$> freshTypeName) args
     (t', s) <- local (mapSchemeEnv $ Map.union argTypes) $ inferType body
     traceM $ "Found type " ++ show t'
     at <- mapMapM instantiateType argTypes
     finalType <- addArgs args (apply s at) t'
     tell $ Map.singleton _name finalType
     return (t', s)


unifyTypes :: Type -> Type -> TypeCheck TypeSubst
unifyTypes t1@(TArrow l1 r1) t2@(TArrow l2 r2) = trace ("unify types " ++ show t1 ++ " with " ++ show t2) $
  do s1 <- unifyTypes l1 l2
     s2 <- unifyTypes (apply s1 r1) (apply s1 r2)
     return $ s2 `compose` s1

unifyTypes t1@(TApply l1 r1) t2@(TApply l2 r2) = trace ("unify types " ++ show t1 ++ " with " ++ show t2) $
  do s1 <- unifyTypes l1 l2
     s2 <- unifyTypes (apply s1 r1) (apply s1 r2)
     return $ s2 `compose` s1

unifyTypes (TList t1) (TList t2) = trace ("unify types " ++ show (TList t1) ++ " with " ++ show (TList t2)) $
  unifyTypes t1 t2
unifyTypes (TVar n) t = trace ("unify type variable " ++ n ++ " with type " ++ show t) $
  bindType n t
unifyTypes t (TVar n) = trace ("unify type variable " ++ n ++ " with type " ++ show t) $
  bindType n t
unifyTypes t1@(TBuiltin n1) t2@(TBuiltin n2)
  | n1 == n2  = return nullSubst
  | otherwise = throwError $ UnificationError t1 t2
unifyTypes t1@(TBuiltin n1) t2@(TName n2)
  | n1 == n2 = return nullSubst
  | otherwise = throwError $ UnificationError t1 t2
unifyTypes t1@(TName n1) t2@(TBuiltin n2)
  | n1 == n2 = return nullSubst
  | otherwise = throwError $ UnificationError t1 t2
unifyTypes (TName n) t = trace ("lookup type name " ++ n ++ " and unify with type " ++ show t) $
  lookupType n t
unifyTypes t (TName n) = trace ("lookup type name " ++ n ++ " and unify with type " ++ show t) $
  lookupType n t
unifyTypes TBottom _ = return nullSubst
unifyTypes _ TBottom = return nullSubst
unifyTypes t1 t2 = trace ("cannot unify " ++ show t1 ++ " with " ++ show t2) $
  throwError $ UnificationError t1 t2

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
     traceM $ "inference for the function returned " ++ show (t1, s1)
     (t2, s2) <- localWithSubst s1 $ inferType e2
     traceM $ "inference for the argument returned " ++ show (t2, s2)
     name     <- freshTypeName
     s3       <- unifyTypes (apply s2 t1) (t2 ^->^ name)
     traceM $ "s1: " ++ show s1
     traceM $ "s2: " ++ show s2
     traceM $ "s3: " ++ show s3
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
     traceM $ "generalized let binding of " ++ show t1' ++ " to " ++ x
     (t2, s2) <- localWithSubst s1 $ local (mapSchemeEnv $ Map.insert x t1') $ inferType e2
     traceM $ "let body returned " ++ show (t2, s2)
     traceM $ "whole let will yield substitution " ++ show (s2 `compose` s1)
     traceM $ "inferred type " ++ show t2 ++ " for let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ""
     return (t2, s2 `compose` s1)

inferType (IEVar x) = trace ("inferType var " ++ x ) $
  do env <- ask
     traceM $ "looking up variable " ++ x ++ " in env " ++ intercalate ", " (map (show . first IEVar) $ Map.toList $ schemeDict env)
     let mt = Map.lookup x $ schemeDict env
         mt' = if isNothing mt
                 then Map.lookup (makePrelude x) $ schemeDict env
                 else mt
     when (isNothing mt') . throwError $ UnboundVariableError x
     t <- instantiateType $ fromJust mt'
     traceM $ "inferred type " ++ show t ++ " for var " ++ x
     return (t, nullSubst)

inferType (ILit (LInt _)) = trace "inferType int literal" $ return (intType, nullSubst)
inferType (ILit LEmptyList) = trace "inferType empty list literal" $ (, nullSubst) <$> preprocessType aListType
inferType (IMatch pats x results) =
  do (t1, s1) <- inferType x
     traceM $ "==inferType IMatch: inferred type " ++ show t1 ++ " for matcher " ++ show x
     (sm, _s2) <- mapAndUnzipM (\pat -> localWithSubst s1 $ checkPattern pat t1) pats
     let s2 = foldr compose nullSubst _s2
     traceM $ "==inferType IMatch: checked patterns {" ++ intercalate ", " (map show pats) ++ "} and got the following identifiers:\n" ++ showList' sm ++ "\n"
     (t2, s3) <- unzip <$> zipWithM (\sm' res -> local (mapSchemeEnv (Map.union sm')) $ localWithSubst (compose s2 s1) $ inferType res) sm results
     let s4 = foldl1 compose s3
     s5 <- foldAdjacentM unifyTypes compose nullSubst t2
     let finalSubst = s5 `compose` s4 `compose` s2 `compose` s1
     traceM $ "==inferType IMatch: final subst is " ++ show finalSubst
     traceM $ "==inferType IMatch: inferred type " ++ show (head t2) ++ " for match with " ++ show x
     return (apply finalSubst $ head t2, finalSubst)

checkPattern :: Pattern -> Type -> TypeCheck (SchemeMap, TypeSubst)
checkPattern (PLit LEmptyList) t = do
  name <- freshTypeName
  s <- unifyTypes t (TList name)
  return (Map.empty, s)
checkPattern (PLit (LInt _)) t = (Map.empty, ) <$> unifyTypes t intType
checkPattern (PVar x) t | x /= "_" = return (Map.singleton x $ ForAll [] t, nullSubst)
checkPattern (PVar _) _ = return (Map.empty, nullSubst)
checkPattern p@(PTVariant name args) t =
  do
    env <- ask
    let ms = Map.lookup name $ schemeDict env
    when (isNothing ms) . throwError $ UnboundVariableError name
    let scheme = fromJust ms
    t' <- instantiateType scheme
    let tArgs = typeArgs t'
    s <- unifyTypes t (typeBody t')
    (maps, substs) <- unzip <$> zipWithM checkPattern args (map (apply s) tArgs)
    let names = concatMap Map.keys maps
    when (length (nub names) /= length names) . throwError $ MultipleBindings (head $ names L.\\ nub names) (show p)
    return (mconcat maps, foldl compose nullSubst substs)

checkPattern (PCons x xs) t =
  do t' <- freshTypeName
     let tl = TList t'
     s1 <- unifyTypes tl t
     (sm1, s2) <- checkPattern x (apply s1 t')
     let s2' = s2 `compose` s1
     (sm2, s3) <- checkPattern xs (apply s2' tl)
     return (sm1 `Map.union` sm2, s3 `compose` s2')
