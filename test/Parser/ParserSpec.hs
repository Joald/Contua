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
     (AST [TypeDecl "R" [] [TypeVariant "R" [TName "Rational"]], TypeDecl "Maybe" [TVar "a"] [TypeVariant "Just" [TVar "a"], TypeVariant "Nothing" []], TypeDecl "Either" [TVar "e", TVar "a"] [TypeVariant "Left" [TVar "e"], TypeVariant "Right" [TVar "a"]], TypeDecl "Expr" [] [TypeVariant "EVar" [TName "Name"], TypeVariant "EInt" [TName "Int"], TypeVariant "ETypeName" [TName "TypeName"], TypeVariant "EAdd" [TName "Expr", TName "Expr"], TypeVariant "ENeg" [TName "Expr"], TypeVariant "ESub" [TName "Expr", TName "Expr"], TypeVariant "EMul" [TName "Expr", TName "Expr"], TypeVariant "EApply" [TName "Expr", TName "Expr"]], TypeDecl "X" [TVar "a", TVar "b"] [TypeVariant "Y" [TVar "a" ^->^ TVar "b"], TypeVariant "Z" [TVar "b" ^->^ TVar "a"]]] [FunDecl ((TVar "b" ^->^ TVar "c") ^->^ TVar "b" ^->^ TVar "c") "f" ["a", "b"] $ EVar "a" ^$^ EVar "b", FunDecl (TVar "m" ^$$^ (TVar "b" ^->^ TVar "c") ^->^ TVar "m" ^$$^ TVar "b" ^->^ TName "Evald" ^$$^ (TVar "m" ^$$^ TVar "c")) "f" ["x", "y"] $ ETypeName "Evald" ^$^ ((EVar "getOut" ^$^ EVar "x") ^$^ EVar "y")])
    shouldParseProgram fooProgram1 (AST [TypeDecl "Foo" [TVar "a", TVar "b"] [TypeVariant "Bar" [TVar "b" ^$$^ TVar "a", TName "Int"], TypeVariant "Bar2" [TVar "a" ^$$^ TName "Int"]]] [])
