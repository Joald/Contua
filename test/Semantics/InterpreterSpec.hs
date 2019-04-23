module Semantics.InterpreterSpec where

import Test.Hspec
import Semantics.InterpreterUtils (getBuiltins)
import Semantics.Interpreter
import Control.Monad.Reader (runReader)
import TypeSystem.TypeDefs
import Parser.TypeDefs
import Semantics.InterpreterUtilsSpec

spec :: Spec
spec = evalTest

evalTest :: SpecWith ()
evalTest = describe "eval" $ do
  it "evaluates nested expressions with shadowing identifiers" $
    runEval (eval $ IELet "x" (ILit $ LInt 1) (IELet "x" (ILit $ LInt 2) (IEVar "x"))) getBuiltins `shouldBe` 2
  it "evaluates match expressions" $
    runEval (eval $ IMatch [PVar "x"] (ILit $ LInt 1) [ILit $ LInt 3]) getBuiltins `shouldBe` 3
