{-# LANGUAGE FlexibleContexts #-}
module Semantics.Interpreter where

import TypeSystem.TypeDefs
import Data.Foldable (find)
import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe (fromJust, fromMaybe)
import Semantics.Builtins
import Semantics.InterpreterUtils
import Semantics.TypeDefs
import Debug.Trace
import Data.List (intercalate)
import Utils
import Control.Monad.State

seqShow :: Show a => a -> String
seqShow x = seq x $ show x

printt :: (MonadState Int m, MonadIO m) => String -> m ()
printt s = do
  x <- get
  liftIO . traceIO $ replicate x ' ' ++ s

interpretAST :: IAST -> IO Value
interpretAST (IAST types fns) = trace "\n\n======\nNow interpreting...\n======\n" $ do
  let Just main = find ((== "main") . ifnName) fns
      env = preprocessEnv $ initFns fns <> initCtors types <> getBuiltins
  traceIO ("env is " ++ showMap env ++ "\nand main is " ++ seqShow main ++ "\n\n")
  evalStateT (runReaderT (runReaderT (eval $ ifnBody main) env) env) 0

incr, decr :: MonadState Int m => m ()
incr = modify (+ 2)
decr = modify (+ (-2))

eval :: IExpr -> Eval Value
eval (IEAbstract x e) = do
  printt ("Evaluating λ" ++ x ++ "." ++ seqShow e)
  incr
  f <- asks (Closure x e)
  printt $ "Found lambda " ++ seqShow f
  decr
  return $ VFun f
eval (IEApply e1 e2) = do
  printt $ "Evaluating application of " ++ seqShow e1 ++ " to " ++ seqShow e2
  incr
  printt $ "Evaluating " ++ seqShow e1
  v1 <- eval e1
  printt $ "Function " ++ seqShow e1 ++ " evaluated to " ++ seqShow v1
  printt $ "Evaluating " ++ seqShow e2
  v2 <- eval e2
  printt $ "Argument " ++ seqShow e2 ++ " evaluated to " ++ seqShow v2
  ret <- apply (getFun v1) v2
  printt $ "Application of " ++ seqShow e1 ++ " to " ++ seqShow e2 ++ " evaluated to " ++ seqShow ret
  decr
  return ret

eval e@(IELet x e1 e2) = do
  printt $ "Evaluating " ++ seqShow e
  incr
  v1 <- eval e1
  printt $ "Let binding for " ++ x ++ " evaluated to " ++ seqShow v1
  ret <- withVar x v1 $ eval e2
  printt $ "Let expression " ++ seqShow e ++ " evaluated to " ++ seqShow ret
  decr
  return ret

eval (IEVar x) = do
  printt $ "Looking up value of variable " ++ x
  incr
  mv <- asks $ Map.lookup x
  mv' <- asks . Map.lookup $ makePrelude x
  mv'' <- asks . Map.lookup $ makeBuiltin x
  let v = fromMaybe (fromMaybe (fromJust mv'') mv') mv
  ret <- if isThunk v
   then withBaseEnv $ eval (getExpr v)
   else return v
  printt $ "Found value " ++ seqShow ret ++ " for variable " ++ seqShow x
  decr
  return ret

eval e@(IMatch pats x results) = do
  printt $ "Evaluating " ++ seqShow e
  incr
  x' <- eval x
  ret <- evalMatch pats x' results
  printt $ "Match expression " ++ seqShow e ++ " evaluated to " ++ seqShow ret
  decr
  return ret

eval (ILit (LInt x)) = printt ("Evaluating integer literal " ++ seqShow x) >> return (VInt x)
eval (ILit LEmptyList) = printt "Evaluation empty list literal []" >> return (VList [])
eval (IIf b e1 e2) = do
  b' <- eval b
  eval $ if (\(VBool x) -> x) b'
    then e1
    else e2

evalMatch :: [Pattern] -> Value -> [IExpr] -> Eval Value
evalMatch (p:ps) x (e:es) = do
  printt $ "Trying pattern " ++ seqShow p ++ " against value " ++ seqShow x
  (b, env) <- p `matches` x
  unless b $ printt $ "Pattern " ++ seqShow p ++ " failed."
  if b
    then do
      printt $ "Pattern " ++ seqShow p ++ " succeeded, evaluating " ++ seqShow e
      incr
      ret <- local (env <>) $ eval e
      printt $ "Pattern branch " ++ seqShow e ++ " evaluated to " ++ seqShow ret
      decr
      return ret
    else evalMatch ps x es

evalMatch _ _ _ = error "patterns were not exhaustive: this can never happen"

matches :: Pattern -> Value -> Eval (Bool, Env)
matches (PVar "_") _ = return (True, Map.empty)
matches (PVar x) v = return (True, Map.singleton x v)
matches (PTVariant pName pats) v@(VAlg vName vals) | pName == vName = do
  printt $ "matching type ctor pattern " ++ pName ++ " with subpatterns " ++ intercalate ", " (map seqShow pats) ++ " against value " ++ seqShow v
  (bools, envs) <- mapAndUnzipM (uncurry matches) $ zip pats vals
  --printt $ "evaluation of tvariant pattern gave env:\n" ++ showMap (mconcat envs) ++ "\n"
  return $ if and bools
    then (True, mconcat envs)
    else (False, Map.empty)
matches (PCons h t) v@(VList (hv : ht)) = do
  printt $ "matching list pattern " ++ seqShow h ++ ":" ++ seqShow t ++ " against value " ++ seqShow v
  (b1, env1) <- matches h hv
  (b2, env2) <- matches t (VList ht)
  return (b1 && b2, if b1 && b2 then env1 <> env2 else Map.empty)
matches (PLit (LInt x)) (VInt y) | x == y = return (True, Map.empty)
matches (PLit LEmptyList) (VList []) = return (True, Map.empty)
matches _ _ = return (False, Map.empty)

partialValue :: [Value] -> Function -> Eval Value
partialValue vs = return . VFun . Partial vs

apply :: Function -> Value -> Eval Value
apply f@(Decl [arg] body) v      = printt ("applying| " ++ seqShow f ++ " to aarg " ++ seqShow v) >>
  withBaseEnv (withVar arg v (eval body))
apply f@(Decl _ _) v             = printt ("applying " ++ seqShow f ++ " to arg " ++ seqShow v) >>
  partialValue [v] f
apply f@(Closure arg body env) v = printt ("applying " ++ seqShow f ++ " to arg " ++ seqShow v) >>
  local (const env) (withVar arg v $ eval body)
apply f@(Builtin x 1) v          = printt ("applying| " ++ seqShow f ++ " to arg " ++ seqShow v) >>
  return (evalBuiltin x [v])
apply f@(Builtin _ _) v          = printt ("applying " ++ seqShow f ++ " to arg " ++ seqShow v) >>
  partialValue [v] f
apply f@(Ctor name 1) v          = printt ("applying| " ++ seqShow f ++ " to arg " ++ seqShow v) >>
  return (VAlg name [v])
apply f@(Ctor _ _) v             = printt ("applying " ++ seqShow f ++ " to arg " ++ seqShow v) >>
  partialValue [v] f
apply f@(Partial args fn) v | length args + 1 == argCount fn = printt ("applying| " ++ seqShow f ++ " to arg " ++ seqShow v) >>
  fullApply fn (args ++ [v])
apply f@(Partial args fn) v      = printt ("applying " ++ seqShow f ++ " to arg " ++ seqShow v) >>
  partialValue (args ++ [v]) fn

fullApply :: Function -> [Value] -> Eval Value
fullApply f@(Decl args body) vs  = printt ("Fully applying " ++ seqShow f ++ " to args " ++ seqShow vs) >>
  withBaseEnv (local (Map.fromList (zip args vs) <>) (eval body))
fullApply f@(Builtin name _) vs  = printt ("Fully applying " ++ seqShow f ++ " to args " ++ seqShow vs) >>
  return (evalBuiltin name vs)
fullApply f@(Ctor name _) vs     = printt ("Fully applying " ++ seqShow f ++ " to args " ++ seqShow vs) >>
  return (VAlg name vs)
fullApply f@(Partial args fn) vs | length args + length vs == argCount fn  = printt ("Fully applying " ++ seqShow f ++ " to args " ++ seqShow vs) >>
  fullApply fn (args ++ vs)
fullApply f@(Partial args fn) vs = printt ("Fully applying " ++ seqShow f ++ " to args " ++ seqShow vs) >>
  partialValue (args ++ vs) fn
fullApply f vs                   = printt ("Fully applying " ++ seqShow f ++ " to args " ++ seqShow vs) >>
  error "fullApply to closure: this can never happen"

argCount :: Function -> Int
argCount (Decl args _) = length args
argCount Closure {} = 1
argCount (Builtin _ x) = x
argCount (Ctor _ x) = x
argCount (Partial args fn) = argCount fn - length args

evalBuiltin :: BuiltinName -> [Value] -> Value
evalBuiltin name [x, y]             | name == addName  = x + y
evalBuiltin name [x, y]             | name == subName  = x - y
evalBuiltin name [x]                | name == negName  = -x
evalBuiltin name [x, y]             | name == mulName  = x * y
evalBuiltin name [x, VList y]       | name == consName = VList $ x : y
evalBuiltin name [VList x, VList y] | name == concName = VList $ x ++ y
evalBuiltin name [VBool x, VBool y] | name == andName  = VBool $ x && y
evalBuiltin name [VBool x, VBool y] | name == orName   = VBool $ x || y
evalBuiltin name [VBool x]          | name == notName  = VBool $ not x
evalBuiltin name [x, y]             | name == eqName   = VBool $ x == y
evalBuiltin name [VInt x, VInt y]   | name == leqName  = VBool $ x <= y
evalBuiltin name [VInt x, VInt y]   | name == lesName  = VBool $ x < y
evalBuiltin name [VBool b, x, y]    | name == ifteName = if b then x else y
evalBuiltin _ _ = error "invalid builtin call: this can never happen"
