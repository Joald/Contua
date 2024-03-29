module Semantics.InterpreterUtilsSpec where

import qualified Data.Map as Map
import Data.Map (Map)

import Test.Hspec
import Control.Monad.Reader (runReaderT)
import TypeSystem.TypeDefs
import Semantics.TypeDefs
import Semantics.Interpreter
import Semantics.InterpreterUtils
import Semantics.Builtins (addName)
import GHC.IO (unsafePerformIO)
import Control.Monad.State

spec :: Spec
spec = envTest

runEval :: Eval a -> Env -> a
runEval x env = unsafePerformIO (evalStateT (runReaderT (runReaderT x env) env) 0)

envTest :: SpecWith ()
envTest =
  describe "Interpreter utils" $
    describe "withVar" $ do
      it "adds new variables" $ do
        runEval (withVar "x" (VInt 2) $ eval $ IEVar "x") Map.empty `shouldBe` 2
        runEval (withVar "djkf" (VInt 6) $ eval $ IEApply (IEApply (IEVar addName) (IEVar "djkf")) (IEVar "djkf")) (Map.fromList [("x", 2)] <> getBuiltins)`shouldBe` 12
        runEval (withVar "x" (VInt 2) $ eval $ IEVar "x") Map.empty `shouldBe` 2
      it "shadows old variables" $
        runEval (withVar "x" (VInt 2) $ eval $ IEVar "x") (Map.singleton "x" (VInt 3)) `shouldBe` 2
