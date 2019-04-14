module Semantics.Builtins where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.List (isPrefixOf)

import Parser.TypeDefs
import TypeSystem.TypeDefs
import TypeSystem.TypeSubstitutable

type BuiltinName = String

builtinPrefix :: BuiltinName
builtinPrefix = "__contua_builtin_"

makeBuiltin :: String -> BuiltinName
makeBuiltin = (builtinPrefix ++)

preludePrefix :: Name
preludePrefix = "prelude."

makePrelude :: Name -> Name
makePrelude = (preludePrefix ++)

isBuiltin, isPrelude :: Name -> Bool
isBuiltin s = builtinPrefix `isPrefixOf` s
isPrelude s = preludePrefix `isPrefixOf` s



addName, subName, negName, mulName, consName, concName, andName, orName, eqName, leqName, notName, ifteName, matchesName :: BuiltinName
addName = makeBuiltin "addition"
subName = makeBuiltin "subtraction"
negName = makeBuiltin "negation"
mulName = makeBuiltin "multiplication"
consName = makeBuiltin "list_construction"
concName = makeBuiltin "concatenation"
andName = makeBuiltin "logical_and"
orName = makeBuiltin "logical_or"
notName = makeBuiltin "logical_not"
eqName = makeBuiltin "equality"
leqName = makeBuiltin "less_than_or_equal"
ifteName = makeBuiltin "if_then_else"
matchesName = makeBuiltin "pattern_match" -- x matches y

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
  , (matchesName, aType ^->^ TPattern aType ^->^ boolType)
  ]

builtinTypes :: Map Name Type
builtinTypes = Map.fromList . zip typeNames $ map TBuiltin typeNames
  where typeNames = ["Int"]