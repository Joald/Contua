module TypeSystem.BuiltinTypes where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)

import TypeSystem.TypeDefs
import TypeSystem.TypeSubstitutable (fv)
import Semantics.Builtins
import Parser.TypeDefs


generalizeBuiltin :: Type -> Scheme
generalizeBuiltin t = ForAll (Set.toList $ fv t) t

typesOfBuiltins :: Map BuiltinName Scheme
typesOfBuiltins = Map.map generalizeBuiltin $ Map.fromList
  [ (addName, binaryType intType)
  , (subName, binaryType intType)
  , (negName, unaryType intType)
  , (mulName, binaryType intType)
  , (consName, aType ^->^ aListType ^->^ aListType)
  , (concName, aListType ^->^ aListType ^->^ aListType)
  , (andName, binaryType boolType)
  , (orName, binaryType boolType)
  , (notName, unaryType boolType)
  , (eqName, aType ^->^ aType ^->^ boolType)
  , (leqName, intType ^->^ intType ^->^ boolType)
  , (ifteName, boolType ^->^ aType ^->^ aType ^->^ aType)
  ]

builtinTypes :: Map Name Type
builtinTypes = Map.fromList . zip typeNames $ map TBuiltin typeNames
  where typeNames = ["Int"]