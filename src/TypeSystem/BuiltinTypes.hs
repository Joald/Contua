module TypeSystem.BuiltinTypes where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)

import TypeSystem.TypeDefs
import TypeSystem.TypeSubstitutable (fv)
import Parser.TypeDefs


generalizeBuiltin :: Type -> Scheme
generalizeBuiltin t = ForAll (Set.toList $ fv t) t


builtinsTypes :: [Type]
builtinsTypes =
  [ binaryType intType -- addName
  , binaryType intType -- subName
  , unaryType intType -- negName
  , binaryType intType -- mulName
  , aType ^->^ aListType ^->^ aListType -- consName
  , aListType ^->^ aListType ^->^ aListType -- concName
  , binaryType boolType -- andName
  , binaryType boolType -- orName
  , unaryType boolType -- notName
  , TNotFunction aType ^->^ TNotFunction aType ^->^ boolType -- eqName
  , intType ^->^ intType ^->^ boolType -- leqName
  , boolType ^->^ aType ^->^ aType ^->^ aType -- ifteName
  ]

builtinTypes :: Map Name Type
builtinTypes = Map.fromList . zip typeNames $ map TBuiltin typeNames
  where typeNames = ["Int"]

builtinTypeDecls :: Map Name TypeDecl
builtinTypeDecls = Map.map (\name -> TypeDecl (unBuiltin name) [] []) builtinTypes