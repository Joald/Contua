module Semantics.Builtins where

import Data.List (isPrefixOf)

import Parser.TypeDefs


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

addName, subName, negName, mulName, consName, concName, andName, orName, notName, eqName, leqName, lesName, ifteName :: BuiltinName
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
lesName = makeBuiltin "less_than"
ifteName = makeBuiltin "if_then_else"

builtinNames :: [BuiltinName]
builtinNames =
  [ addName
  , subName
  , negName
  , mulName
  , consName
  , concName
  , andName
  , orName
  , notName
  , eqName
  , leqName
  , lesName
  , ifteName
  ]