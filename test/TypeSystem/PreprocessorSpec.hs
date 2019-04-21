module TypeSystem.PreprocessorSpec where


import Test.Hspec
import Parser.TypeDefs
import TypeSystem.TypeDefs
import TypeSystem.Preprocessor
import Semantics.Builtins

spec :: Spec
spec = desugarerTest

desugarerTest :: SpecWith ()
desugarerTest = describe "Desugarer" $
  it "desugars expressions" $ do
    desugar (EVar "a" ^+^ EVar "b") `shouldBe` (IEVar addName ^^$ IEVar "a" ^^$ IEVar "b")
    desugar (EMatch (EVar "x") [(EInt 1, EInt 10), (EInt 2, EInt 20)]) `shouldBe` (IMatch [PLit $ LInt 1, PLit $ LInt 2] (IEVar "x") [ILit $ LInt 10, ILit $ LInt 20])
