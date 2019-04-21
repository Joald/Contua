module Semantics.Interpreter where

import Parser.TypeDefs
import TypeSystem.TypeDefs
import Data.Foldable (find)
import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe (fromJust, fromMaybe)
import Semantics.Builtins
import TypeSystem.BuiltinTypes
import Semantics.TypeDefs
import Debug.Trace

interpretAST :: IAST -> Value
interpretAST (IAST types fns) =
  let Just main = find ((== "main") . ifnName) fns
      env = initEnv fns <> initCtors types <> getBuiltins
    in runReader (eval $ ifnBody main) env

getBuiltins :: Env
getBuiltins =
  Map.fromList
    . zip builtinNames
    . map VFun
    . zipWith Builtin builtinNames $ map tLength builtinsTypes

initCtors :: [ITypeDecl] -> Env
initCtors = Map.fromList . concatMap (\(TypeDecl _ _ variants) ->
  map (\(TypeVariant name args) ->
    (name, VFun . Ctor name $ length args)) variants)

initEnv :: [IFnDecl] -> Env
initEnv = foldMap declToEnv
  where
    declToEnv (IFnDecl _ _ name' args' body') = Map.singleton name' . VFun $ Decl args' body'

withVar :: Name -> Value -> Eval a -> Eval a
withVar k v = local (<> Map.singleton k v)

eval :: IExpr -> Eval Value
eval (IEAbstract x e) = VFun <$> asks (Closure x e)
eval (IEApply e1 e2) =
  do v1 <- eval e1
     v2 <- eval e2
     apply (getFun v1) v2
eval (IELet x e1 e2) =
  do v1 <- eval e1
     withVar x v1 $ eval e2
eval (IEVar x) =
  do mv <- asks $ Map.lookup x
     mv' <- asks . Map.lookup $ makePrelude x
     mv'' <- asks . Map.lookup $ makeBuiltin x
     return $ fromMaybe (fromMaybe (fromJust mv'') mv') mv
eval (IMatch pats x results) = eval x >>= \x' -> evalMatch pats x' results
eval (ILit (LInt x)) = return $ VInt x
eval (ILit LEmptyList) = return $ VList []

evalMatch :: [Pattern] -> Value -> [IExpr] -> Eval Value
evalMatch (p:ps) x (e:es) =
 do (b, env) <- p `matches` x
    if b
      then local (<> env) $ eval e
      else evalMatch ps x es
evalMatch _ _ _ = error "patterns were not exhaustive: this can never happen"

matches :: Pattern -> Value -> Eval (Bool, Env)
matches (PVar x) v = return (True, if x == "_" then Map.empty else Map.singleton x v)
matches (PTVariant pName pats) (VAlg vName vals) | pName == vName =
  do (bools, envs) <- mapAndUnzipM (uncurry matches) $ zip pats vals
     return $ if and bools
       then (True, mconcat envs)
       else (False, Map.empty)
matches (PCons h t) (VList (hv : ht)) =
  do (b1, env1) <- matches h hv
     (b2, env2) <- matches t (VList ht)
     return (b1 && b2, if b1 && b2 then env1 <> env2 else Map.empty)
matches (PLit (LInt x)) (VInt y) | x == y = return (True, Map.empty)
matches (PLit LEmptyList) (VList []) = return (True, Map.empty)
matches _ _ = return (False, Map.empty)

partialValue :: [Value] -> Function -> Eval Value
partialValue vs = return . VFun . Partial vs

apply :: Function -> Value -> Eval Value
apply (Decl [arg] body) v      = withVar arg v $ eval body
apply f@(Decl _ _) v           = partialValue [v] f
apply (Closure arg body env) v = local (const env) $ withVar arg v $ eval body
apply (Builtin x 1) v          = return $ evalBuiltin x [v]
apply f@(Builtin _ _) v        = partialValue [v] f
apply (Ctor name 1) v          = return $ VAlg name [v]
apply f@(Ctor _ _) v           = partialValue [v] f
apply (Partial args fn) v | length args + 1 == argCount fn = fullApply fn (args ++ [v])
apply (Partial args fn) v      = partialValue (args ++ [v]) fn

fullApply :: Function -> [Value] -> Eval Value
fullApply (Decl args body) vs = local (<> Map.fromList (zip args vs)) $ eval body
fullApply (Builtin name _) vs = return $ evalBuiltin name vs
fullApply (Ctor name _) vs = return $ VAlg name vs
fullApply (Partial args fn) vs | length args + length vs == argCount fn = fullApply fn (args ++ vs)
fullApply (Partial args fn) vs = partialValue (args ++ vs) fn
fullApply _ _ = error "fullApply to closure: this can never happen"

argCount :: Function -> Int
argCount (Decl args _) = length args
argCount Closure {} = 1
argCount (Builtin _ x) = x
argCount (Ctor _ x) = x
argCount (Partial args fn) = argCount fn - length args

evalBuiltin :: BuiltinName -> [Value] -> Value
evalBuiltin name [x, y]             | name == addName  = x + y
evalBuiltin name [x, y]             | name == subName  = x - y
evalBuiltin name [x, VList y]       | name == consName = VList $ x : y
evalBuiltin name [VList x, VList y] | name == concName = VList $ x ++ y
evalBuiltin name [x]                | name == negName  = -x
evalBuiltin name [VBool x, VBool y] | name == andName  = VBool $ x && y
evalBuiltin name [VBool x, VBool y] | name == orName   = VBool $ x || y
evalBuiltin name [VBool x]          | name == notName  = VBool $ not x
evalBuiltin name [x, y]             | name == eqName   = VBool $ x == y
evalBuiltin name [VInt x, VInt y]   | name == leqName  = VBool $ x <= y
evalBuiltin name [VBool b, x, y]    | name == ifteName = if b then x else y
evalBuiltin _ _ = error "invalid builtin call: this can never happen"
