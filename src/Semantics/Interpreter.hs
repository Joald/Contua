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

interpretAST :: IAST -> IO Value
interpretAST (IAST types fns) = trace "\n\n======\nNow interpreting...\n======\n" $ do
  let Just main = find ((== "main") . ifnName) fns
      env = preprocessEnv $ initFns fns <> initCtors types <> getBuiltins
--  env' <- evaluateConstants env
  traceIO ("env is " ++ showMap env ++ "\nand main is " ++ show main ++ "\n\n")
  runReaderT (eval $ ifnBody main) env

evaluateConstants :: Env -> IO Env
evaluateConstants env = trace (showMap env) forMapWithKeyM env $  \name v -> case v of
  VFun (Decl [] body) -> trace (show body ++ " matched the pattern.") $
    if name /= "main" then runReaderT (eval body) env else return v
  VFun (Ctor _name 0) -> trace (_name ++ " was reduced.") return $ VAlg _name []
  anything -> trace (show anything ++ " did not match the pattern.") return anything


eval :: IExpr -> Eval Value
eval (IEAbstract x e) = trace ("Evaluating λ" ++ x ++ "." ++ show e) VFun <$> asks (Closure x e)
eval (IEApply e1 e2) = trace ("Evaluating application of " ++ show e1 ++ " to " ++ show e2) $
  do v1 <- eval e1
     lift $ traceIO $ show e1 ++ " evaluated to " ++ show v1
     v2 <- eval e2
     lift $ traceIO $ show e2 ++ " evaluated to " ++ show v2
     apply (getFun v1) v2
eval e@(IELet x e1 e2) = trace ("Evaluating " ++ show e) $
  do v1 <- eval e1
     lift $ traceIO $ "Let binding " ++ x ++ " evaluated to " ++ show v1
     withVar x v1 $ eval e2
eval (IEVar x) = trace ("Looking up value of variable " ++ x) $
  do mv <- asks $ Map.lookup x
     mv' <- asks . Map.lookup $ makePrelude x
     mv'' <- asks . Map.lookup $ makeBuiltin x
     let v = fromMaybe (fromMaybe (fromJust mv'') mv') mv
     lift $ traceIO $ "Found value " ++ show v ++ " for variable " ++ show x
     if isThunk v
       then eval (getExpr v)
       else return v

eval e@(IMatch pats x results) = trace ("Evaluating " ++ show e) eval x >>= \x' -> evalMatch pats x' results
eval (ILit (LInt x)) = trace ("Evaluating integer literal " ++ show x) return $ VInt x
eval (ILit LEmptyList) = trace "Evaluation empty list literal []" return $ VList []


evalMatch :: [Pattern] -> Value -> [IExpr] -> Eval Value
evalMatch (p:ps) x (e:es) =
 do lift $ traceIO $ "Trying pattern " ++ show p ++ " against value " ++ show x
    (b, env) <- p `matches` x
    if b
      then local (env <>) $ eval e
      else evalMatch ps x es
evalMatch _ _ _ = do
  lift $ putStrLn "dupa xd"
  error "patterns were not exhaustive: this can never happen"

matches :: Pattern -> Value -> Eval (Bool, Env)
matches (PVar "_") _ = return (True, Map.empty)
matches (PVar x) v = return (True, Map.singleton x v)
matches (PTVariant pName pats) v@(VAlg vName vals) | pName == vName =
  do lift $ traceIO $ "evaluating type ctor pattern " ++ pName ++ " with subpatterns " ++ intercalate ", " (map show pats) ++ " against value " ++ show v
     (bools, envs) <- mapAndUnzipM (uncurry matches) $ zip pats vals
     liftIO $ traceIO $ "evaluation of tvariant pattern gave env:\n" ++ showMap (mconcat envs) ++ "\n"
     return $ if and bools
       then (True, mconcat envs)
       else (False, Map.empty)
matches (PCons h t) v@(VList (hv : ht)) =
  do lift $ traceIO $ "evaluation list pattern " ++ show h ++ ":" ++ show t ++ " against value " ++ show v
     (b1, env1) <- matches h hv
     (b2, env2) <- matches t (VList ht)
     return (b1 && b2, if b1 && b2 then env1 <> env2 else Map.empty)
matches (PLit (LInt x)) (VInt y) | x == y = return (True, Map.empty)
matches (PLit LEmptyList) (VList []) = return (True, Map.empty)
matches _ _ = return (False, Map.empty)

partialValue :: [Value] -> Function -> Eval Value
partialValue vs = return . VFun . Partial vs

apply :: Function -> Value -> Eval Value
apply f@(Decl [arg] body) v      = trace ("applying| " ++ show f ++ " to aarg " ++ show v) withVar arg v $ eval body
apply f@(Decl _ _) v           = trace ("applying " ++ show f ++ " to arg " ++ show v) partialValue [v] f
apply f@(Closure arg body env) v = trace ("applying " ++ show f ++ " to arg " ++ show v) local (const env) $ withVar arg v $ eval body
apply f@(Builtin x 1) v          = trace ("applying| " ++ show f ++ " to arg " ++ show v) return $ evalBuiltin x [v]
apply f@(Builtin _ _) v        = trace ("applying " ++ show f ++ " to arg " ++ show v) partialValue [v] f
apply f@(Ctor name 1) v          = trace ("applying| " ++ show f ++ " to arg " ++ show v) return $ VAlg name [v]
apply f@(Ctor _ _) v           = trace ("applying " ++ show f ++ " to arg " ++ show v) partialValue [v] f
apply f@(Partial args fn) v | length args + 1 == argCount fn = trace ("applying| " ++ show f ++ " to arg " ++ show v) fullApply fn (args ++ [v])
apply f@(Partial args fn) v      = trace ("applying " ++ show f ++ " to arg " ++ show v) partialValue (args ++ [v]) fn

fullApply :: Function -> [Value] -> Eval Value
fullApply f@(Decl args body) vs  = trace ("Fully applying " ++ show f ++ " to args " ++ show vs)local (Map.fromList (zip args vs) <>) $ eval body
fullApply f@(Builtin name _) vs  = trace ("Fully applying " ++ show f ++ " to args " ++ show vs)return $ evalBuiltin name vs
fullApply f@(Ctor name _) vs  = trace ("Fully applying " ++ show f ++ " to args " ++ show vs)return $ VAlg name vs
fullApply f@(Partial args fn) vs | length args + length vs == argCount fn  = trace ("Fully applying " ++ show f ++ " to args " ++ show vs)fullApply fn (args ++ vs)
fullApply f@(Partial args fn) vs  = trace ("Fully applying " ++ show f ++ " to args " ++ show vs)partialValue (args ++ vs) fn
fullApply f vs  = trace ("Fully applying " ++ show f ++ " to args " ++ show vs)error "fullApply to closure: this can never happen"

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
