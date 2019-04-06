module TypeSystem.PreprocessorSpec where


import Test.Hspec
import Parser.TypeDefs
import TypeSystem.TypeDefs
import TypeSystem.Preprocessor
import Semantics.Builtins

spec :: Spec
spec = desugarerTest

desugarerTest = describe "Desugarer" $
  it "desugars expressions" $ do
    desugar (EVar "a" ^+^ EVar "b") `shouldBe` (IEVar addName ^^$ IEVar "a" ^^$ IEVar "b")
    desugar (EMatch (EVar "x") [(EInt 1, EInt 10), (EInt 2, EInt 20)]) `shouldBe` IEApply (IEApply (IEApply (IEVar ifteName) (IEApply (IEApply (IEVar matchesName) (IEVar "x")) (ILit $ LInt 1))) (ILit $ LInt 10)) (IEApply (IEApply (IEApply (IEVar ifteName) (IEApply (IEApply (IEVar matchesName) (IEVar "x")) (ILit $ LInt 2))) (ILit $ LInt 20)) (ILit LError))
