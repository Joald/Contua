module Parser.ParserSpec where

import TestUtils

import Test.Hspec
import Parser.TypeDefs
import Control.Monad.Trans.Maybe (MaybeT)

spec :: Spec
spec = programParserTest

fooProgram1, fooProgram1', fooProgram2, stateTProgram :: String
fooProgram1 = "type Foo a b = Bar (b a) Int | Bar2 (a Int);"
fooProgram1' = "type Foo b a = Bar (b a) Int | Bar2 (a Int);"
stateTProgram = "type StateT s m a = StateT (s -> m a);"
fooProgram2 = fooProgram1 ++ stateTProgram
  ++ "type Barium a b c d e f = ReaderT a (WriterT b (StateT c d) e) f;"



programParserTest :: Spec
programParserTest = describe "Program parser" $
  it "parses programs" $ do
    shouldParseProgram "type R = R Rational;\n\
     \type Maybe a = Just a | Nothing;\n\
     \type Either e a = Left e | Right a;\n\
     \type Expr = EVar Name | EInt Int | ETypeName TypeName | EAdd Expr Expr | ENeg Expr | ESub Expr Expr | EMul Expr Expr | EApply Expr Expr;\n\
     \type X a b = Y (a -> b) | Z (b -> a);\n\
     \(b -> c) -> (b -> c) :: f a b = a b;\n\
     \m (b -> c) -> m b -> Evald (m c) :: f x y = Evald ((getOut x) y);\n"
     (AST [TypeDecl (TypeName "R") [] [TypeVariant (TypeName "R") [mkTCtor "Rational"]], TypeDecl (TypeName "Maybe") [TPoly "a"] [TypeVariant (TypeName "Just") [TPoly "a"], TypeVariant (TypeName "Nothing") []], TypeDecl (TypeName "Either") [TPoly "e", TPoly "a"] [TypeVariant (TypeName "Left") [TPoly "e"], TypeVariant (TypeName "Right") [TPoly "a"]], TypeDecl (TypeName "Expr") [] [TypeVariant (TypeName "EVar") [mkTCtor "Name"], TypeVariant (TypeName "EInt") [mkTCtor "Int"], TypeVariant (TypeName "ETypeName") [mkTCtor "TypeName"], TypeVariant (TypeName "EAdd") [mkTCtor "Expr", mkTCtor "Expr"], TypeVariant (TypeName "ENeg") [mkTCtor "Expr"], TypeVariant (TypeName "ESub") [mkTCtor "Expr", mkTCtor "Expr"], TypeVariant (TypeName "EMul") [mkTCtor "Expr", mkTCtor "Expr"], TypeVariant (TypeName "EApply") [mkTCtor "Expr", mkTCtor "Expr"]], TypeDecl (TypeName "X") [TPoly "a", TPoly "b"] [TypeVariant (TypeName "Y") [TPoly "a" ^->^ TPoly "b"], TypeVariant (TypeName "Z") [TPoly "b" ^->^ TPoly "a"]]] [FunDecl ((TPoly "b" ^->^ TPoly "c") ^->^ TPoly "b" ^->^ TPoly "c") "f" ["a", "b"] $ EVar "a" ^$^ EVar "b", FunDecl (TPoly "m" ^$$^ (TPoly "b" ^->^ TPoly "c") ^->^ TPoly "m" ^$$^ TPoly "b" ^->^ mkTCtor "Evald" ^$$^ (TPoly "m" ^$$^ TPoly "c")) "f" ["x", "y"] $ ETypeName (TypeName "Evald") ^$^ ((EVar "getOut" ^$^ EVar "x") ^$^ EVar "y")])
    shouldParseProgram fooProgram1 (AST [TypeDecl (TypeName "Foo") [TPoly "a", TPoly "b"] [TypeVariant (TypeName "Bar") [TPoly "b" ^$$^ TPoly "a", TCtor $ TypeName "Int"], TypeVariant (TypeName "Bar2") [TPoly "a" ^$$^ TCtor (TypeName "Int")]]] [])
