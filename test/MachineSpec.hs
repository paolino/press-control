module MachineSpec where

import Test.Hspec

spec :: Spec
spec = do
    describe "add" $ do
        it "adds two numbers" $ do
            (+) 1 2 `shouldBe` 3
