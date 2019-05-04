module Semantics.InterpreterUtils where

import qualified Data.Map as Map
import Semantics.TypeDefs
import TypeSystem.TypeDefs
import Parser.TypeDefs
import Semantics.Builtins (builtinNames)
import Control.Monad.Reader
import TypeSystem.BuiltinTypes (builtinsTypes)

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

initFns :: [IFnDecl] -> Env
initFns = foldMap declToEnv
  where declToEnv (IFnDecl _ _ name' args' body') = Map.singleton name' $ VFun $ Decl args' body'

withVar :: Name -> Value -> Eval a -> Eval a
withVar k v = local (Map.insert k v)

withBaseEnv :: Eval a -> Eval a
withBaseEnv m = do
  baseEnv <- lift ask
  local (const baseEnv) m

preprocessEnv :: Env -> Env
preprocessEnv = Map.map reduceCtors -- . Map.map evalConstants

reduceCtors :: Value -> Value
reduceCtors (VList l) = VList $ map reduceCtors l
reduceCtors (VAlg name l) = VAlg name $ map reduceCtors l
reduceCtors (VFun (Partial args fn)) = VFun $ Partial (map reduceCtors args) (getFun $ reduceCtors $ VFun fn)
reduceCtors (VFun (Ctor name 0)) = VAlg name []
reduceCtors (VFun (Decl [] e)) = VExpr e
reduceCtors anything = anything

isThunk :: Value -> Bool
isThunk (VExpr _) = True
isThunk _ = False

getExpr :: Value -> IExpr
getExpr (VExpr e) = e

