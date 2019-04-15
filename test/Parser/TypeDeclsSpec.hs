module Parser.TypeDeclsSpec where


import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Parser.TypeDefs
import Parser.TypeDecls
import Parser.Expr
import Parser.Parser
import TestUtils

spec :: Spec
spec = do
  typeParserTest
  declarationParserTest

typeParserTest = describe "Type parser" $ do
  it "parses type constructors" $ do
    shouldParseType "Abcds" $ TName "Abcds"
    shouldParseType "A123123bsdf324fds325" $ TName "A123123bsdf324fds325"
    shouldParseType "AAAAAAAAAAAAAAAAAAAAAAAA" $ TName "AAAAAAAAAAAAAAAAAAAAAAAA"
  it "parses function types" $ do
    shouldParseType "x -> y" $ TVar "x" ^->^ TVar "y"
    shouldParseType "x -> y -> y" $ TVar "x" ^->^ TVar "y" ^->^ TVar "y"
  it "parses type application" $ do
    shouldParseType "x a" $ TVar "x" ^$$^ TVar "a"
    shouldParseType "a b c d e " $ TVar "a" ^$$^ TVar "b" ^$$^ TVar "c" ^$$^ TVar "d" ^$$^ TVar "e"
    shouldParseType "x a -> Int" $ TVar "x" ^$$^ TVar "a" ^->^ TName "Int"
  it "parses different type parenthesizations" $ do
    shouldParseType "(a -> b) -> (b -> c) -> a -> c" $ (TVar "a" ^->^ TVar "b") ^->^ (TVar "b" ^->^ TVar "c") ^->^ TVar "a" ^->^ TVar "c"
    shouldParseType "m a -> m (a -> m b) -> m b" $ TVar "m" ^$$^ TVar "a" ^->^ TVar "m" ^$$^ (TVar "a" ^->^ TVar "m" ^$$^ TVar "b") ^->^ TVar "m" ^$$^ TVar "b"
    shouldParseType "[a]" $ TList $ TVar "a"
    shouldParseType "[abcd]" $ TList $ TVar "abcd"
    shouldParseType "[Int]" $ TList $ TName "Int"
    shouldParseType "[a -> Int]" $ TList $ TVar "a" ^->^ TName "Int"


declarationParserTest = describe "Declaration parser" $ do
  it "parses type declarations" $ do
    shouldParseTypeDecl "type R = R Rational;" $ TypeDecl "R" [] [TypeVariant "R" [TName "Rational"]]
    shouldParseTypeDecl "type Maybe a = Just a | Nothing;" $ TypeDecl  "Maybe" [TVar "a"] [TypeVariant  "Just" [TVar "a"], TypeVariant  "Nothing" []]
    shouldParseTypeDecl "type Either e a = Left e | Right a;" $ TypeDecl  "Either" [TVar "e", TVar "a"] [TypeVariant  "Left" [TVar "e"], TypeVariant  "Right" [TVar "a"]]
    shouldParseTypeDecl "type Expr = EVar Name | EInt Int | ETypeName TypeName | EAdd Expr Expr | ENeg Expr | ESub Expr Expr | EMul Expr Expr | EApply Expr Expr;" $ TypeDecl  "Expr" [] [TypeVariant "EVar" [TName "Name"], TypeVariant  "EInt" [TName "Int"], TypeVariant  "ETypeName" [TName "TypeName"], TypeVariant  "EAdd" [TName "Expr", TName "Expr"], TypeVariant  "ENeg" [TName "Expr"], TypeVariant  "ESub" [TName "Expr", TName "Expr"], TypeVariant  "EMul" [TName "Expr", TName "Expr"], TypeVariant  "EApply" [TName "Expr", TName "Expr"]]
    shouldParseTypeDecl "type X a b = Y (a -> b) | Z (b -> a);" $ TypeDecl "X" [TVar "a", TVar "b"] [TypeVariant "Y" [TVar "a" ^->^ TVar "b"], TypeVariant "Z" [TVar "b" ^->^ TVar "a"]]
    parse typeDecl "" `shouldFailOn` "type X a b = Y a -> b;" --err 18 (utok '-' <> etok '(' <> etok '|' <> eeof <> etoks "identifier" <> "typename")
  it "parses function declarations" $ do
    shouldParseFunDecl "(b -> c) -> (b -> c) :: f a b = a b;" $ FunDecl ((TVar "b" ^->^ TVar "c") ^->^ TVar "b" ^->^ TVar "c") "f" ["a", "b"] $ EVar "a" ^$^ EVar "b"
    shouldParseFunDecl "m (b -> c) -> m b -> Evald (m c) :: f x y = Evald ((getOut x) y);" $ FunDecl (TVar "m" ^$$^ (TVar "b" ^->^ TVar "c") ^->^ TVar "m" ^$$^ TVar "b" ^->^ TName "Evald" ^$$^ (TVar "m" ^$$^ TVar "c")) "f" ["x", "y"] $ ETypeName "Evald" ^$^ ((EVar "getOut" ^$^ EVar "x") ^$^ EVar "y")

