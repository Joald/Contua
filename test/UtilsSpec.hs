{-# LANGUAGE FlexibleContexts #-}
module UtilsSpec where

import           Control.Monad.Reader.Class
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Test.Hspec

import           Data.Maybe
import           Utils

leTest :: MonadReader (Map Int Int) m => Int -> m Int
leTest 1 = fromMaybe (-2137) . Map.lookup 1 <$> ask
leTest x = localEnv 1 (x + 1) $ leTest 1


spec :: Spec
spec = describe "generic utils" $
  describe "localEnv" $
    it "updates environments" $
      leTest 69 Map.empty `shouldBe` 70

--localEnvTest :: KindChecka
