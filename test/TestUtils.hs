module TestUtils where

import Control.Monad.Except

import Data.Either

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec


import Parser.TypeDefs
import Parser.Expr
import Parser.TypeDecls
import Parser.Parser

import TypeSystem.KindChecker
import TypeSystem.Substitutable

shouldParseExpr :: String -> Expr -> Expectation
shouldParseExpr = shouldParse . parse expr ""

shouldParseType :: String -> Type -> Expectation
shouldParseType = shouldParse . parse type_ ""

shouldParseTypeDecl :: String -> TypeDecl -> Expectation
shouldParseTypeDecl = shouldParse . parse typeDecl ""

shouldParseFunDecl :: String -> FunDecl -> Expectation
shouldParseFunDecl = shouldParse . parse funDecl ""

shouldParseProgram :: String -> AST -> Expectation
shouldParseProgram = shouldParse . parse program ""

parseHelper :: a -> Parser a -> String -> a
parseHelper defaultValue parser = fromRight defaultValue . parse parser ""


parseTypeDecl :: String -> TypeDecl
parseTypeDecl = parseHelper (TypeDecl (TypeName "Parse error") [] []) typeDecl
parseTypeDecls :: String -> [TypeDecl]
parseTypeDecls = parseHelper [TypeDecl (TypeName "Parse error") [] []] $ many typeDecl
parseType :: String -> Type
parseType = parseHelper (TCtor (TypeName "Parse error")) type_
parseAST :: String -> AST
parseAST = parseHelper (AST [] []) program

fooTest = "type Foo b a = Bar (b a) Int | Bar2 (a Int);"
fooTest' = "type Foo a b = Bar (b a) Int | Bar2 (a Int);"

extract :: a -> Except b a -> a
extract defaultValue = fromRight defaultValue . runExcept
extract' :: (b -> a) -> Except b a -> a
extract' f = either f id . runExcept

defaultKind :: (Kind, KindSubst)
defaultKind = (KUnknown "kind error", nullSubst)