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
    shouldParseType "Abcds" $ mkTCtor "Abcds"
    shouldParseType "A123123bsdf324fds325" $ mkTCtor "A123123bsdf324fds325"
    shouldParseType "AAAAAAAAAAAAAAAAAAAAAAAA" $ mkTCtor "AAAAAAAAAAAAAAAAAAAAAAAA"
  it "parses function types" $ do
    shouldParseType "x -> y" $ TPoly "x" ^->^ TPoly "y"
    shouldParseType "x -> y -> y" $ TPoly "x" ^->^ TPoly "y" ^->^ TPoly "y"
  it "parses type application" $ do
    shouldParseType "x a" $ TPoly "x" ^$$^ TPoly "a"
    shouldParseType "a b c d e " $ TPoly "a" ^$$^ TPoly "b" ^$$^ TPoly "c" ^$$^ TPoly "d" ^$$^ TPoly "e"
    shouldParseType "x a -> Int" $ TPoly "x" ^$$^ TPoly "a" ^->^ mkTCtor "Int"
  it "parses different type parenthesizations" $ do
    shouldParseType "(a -> b) -> (b -> c) -> a -> c" $ (TPoly "a" ^->^ TPoly "b") ^->^ (TPoly "b" ^->^ TPoly "c") ^->^ TPoly "a" ^->^ TPoly "c"
    shouldParseType "m a -> m (a -> m b) -> m b" $ TPoly "m" ^$$^ TPoly "a" ^->^ TPoly "m" ^$$^ (TPoly "a" ^->^ TPoly "m" ^$$^ TPoly "b") ^->^ TPoly "m" ^$$^ TPoly "b"
    shouldParseType "[a]" $ TList $ TPoly "a"
    shouldParseType "[abcd]" $ TList $ TPoly "abcd"
    shouldParseType "[Int]" $ TList $ mkTCtor "Int"
    shouldParseType "[a -> Int]" $ TList $ TPoly "a" ^->^ mkTCtor "Int"


declarationParserTest = describe "Declaration parser" $ do
  it "parses type declarations" $ do
    shouldParseTypeDecl "type R = R Rational;" $ TypeDecl (TypeName "R") [] [TypeVariant (TypeName "R") [mkTCtor "Rational"]]
    shouldParseTypeDecl "type Maybe a = Just a | Nothing;" $ TypeDecl (TypeName "Maybe") [TPoly "a"] [TypeVariant (TypeName "Just") [TPoly "a"], TypeVariant (TypeName "Nothing") []]
    shouldParseTypeDecl "type Either e a = Left e | Right a;" $ TypeDecl (TypeName "Either") [TPoly "e", TPoly "a"] [TypeVariant (TypeName "Left") [TPoly "e"], TypeVariant (TypeName "Right") [TPoly "a"]]
    shouldParseTypeDecl "type Expr = EVar Name | EInt Int | ETypeName TypeName | EAdd Expr Expr | ENeg Expr | ESub Expr Expr | EMul Expr Expr | EApply Expr Expr;" $ TypeDecl (TypeName "Expr") [] [TypeVariant (TypeName "EVar") [mkTCtor "Name"], TypeVariant (TypeName "EInt") [mkTCtor "Int"], TypeVariant (TypeName "ETypeName") [mkTCtor "TypeName"], TypeVariant (TypeName "EAdd") [mkTCtor "Expr", mkTCtor "Expr"], TypeVariant (TypeName "ENeg") [mkTCtor "Expr"], TypeVariant (TypeName "ESub") [mkTCtor "Expr", mkTCtor "Expr"], TypeVariant (TypeName "EMul") [mkTCtor "Expr", mkTCtor "Expr"], TypeVariant (TypeName "EApply") [mkTCtor "Expr", mkTCtor "Expr"]]
    shouldParseTypeDecl "type X a b = Y (a -> b) | Z (b -> a);" $ TypeDecl (TypeName "X") [TPoly "a", TPoly "b"] [TypeVariant (TypeName "Y") [TPoly "a" ^->^ TPoly "b"], TypeVariant (TypeName "Z") [TPoly "b" ^->^ TPoly "a"]]
    parse typeDecl "" `shouldFailOn` "type X a b = Y a -> b;" --err 18 (utok '-' <> etok '(' <> etok '|' <> eeof <> etoks "identifier" <> "typename")
  it "parses function declarations" $ do
    shouldParseFunDecl "(b -> c) -> (b -> c) :: f a b = a b;" $ FunDecl ((TPoly "b" ^->^ TPoly "c") ^->^ TPoly "b" ^->^ TPoly "c") "f" ["a", "b"] $ EVar "a" ^$^ EVar "b"
    shouldParseFunDecl "m (b -> c) -> m b -> Evald (m c) :: f x y = Evald ((getOut x) y);" $ FunDecl (TPoly "m" ^$$^ (TPoly "b" ^->^ TPoly "c") ^->^ TPoly "m" ^$$^ TPoly "b" ^->^ mkTCtor "Evald" ^$$^ (TPoly "m" ^$$^ TPoly "c")) "f" ["x", "y"] $ ETypeName (TypeName "Evald") ^$^ ((EVar "getOut" ^$^ EVar "x") ^$^ EVar "y")

