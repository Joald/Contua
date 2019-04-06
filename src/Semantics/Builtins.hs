module Semantics.Builtins where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (isPrefixOf)

import Parser.TypeDefs

type BuiltinName = String

builtinPrefix :: BuiltinName
builtinPrefix = "__contua_builtin_"

makeBuiltin :: String -> BuiltinName
makeBuiltin = (builtinPrefix ++)

isBuiltin :: String -> Bool
isBuiltin s = builtinPrefix `isPrefixOf` s

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


builtinTypes :: Map BuiltinName Type
builtinTypes = Map.fromList
  [ (addName, binaryType intType)
  , (subName, binaryType intType)
  , (negName, unaryType intType)
  , (mulName, binaryType intType)
  , (consName, aType ^->^ aListType ^->^ aListType)
  , (concName, aListType ^->^ aListType ^->^ aListType)
  , (andName, binaryType boolType)
  , (orName, binaryType boolType)
  , (notName, unaryType boolType)
  , (eqName, intType ^->^ intType ^->^ boolType)
  , (leqName, intType ^->^ intType ^->^ boolType)
  , (ifteName, boolType ^->^ intType ^->^ intType ^->^ intType ^->^ intType)
  , (matchesName, aType ^->^ TPattern aType ^->^ boolType)
  ]