module OperationSpec where

import Operation
    ( Operation (Add, Move, Remove)
    , operationsToChangeRecipe
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Tool

spec :: Spec
spec = do
    let operations = operationsToChangeRecipe
    describe "operations to change recipe" $ do
        it "empty recipe to empty recipe"
            $ operations [] []
            `shouldBe` []
        it "empty recipe to one tool"
            $ operations [] [placed 0 1]
            `shouldBe` [Add $ placed 0 1]
        it "one tool to empty recipe"
            $ operations [placed 0 1] []
            `shouldBe` [Remove $ placed 0 1]
        it "one tool to one tool"
            $ operations [placed 0 1] [placed 0 1]
            `shouldBe` []
        it "one tool to one tool, different position"
            $ operations [placed 0 1] [placed 1 2]
            `shouldBe` [Remove $ placed 0 1, Add $ placed 1 2]
        it "one tool to one tool, different tool"
            $ operations [placed 0 1] [placed 0 2]
            `shouldBe` [Remove $ placed 0 1, Add $ placed 0 2]
        it "one tool to one tool, different tool and position"
            $ operations [placed 0 1] [placed 1 2]
            `shouldBe` [Remove $ placed 0 1, Add $ placed 1 2]
        it "one tool to two tools, different tool and position"
            $ operations [placed 0 1] [placed 0 1, placed 1 1]
            `shouldBe` [Add $ placed 1 1]
        it "two tools to one tool, different tool and position"
            $ operations [placed 0 1, placed 1 1] [placed 0 1]
            `shouldBe` [Remove $ placed 1 1]
        it "two tools to one tool, different tool and position"
            $ operations [placed 0 1, placed 1 1] [placed 1 1]
            `shouldBe` [Remove $ placed 0 1]
        it "are always sorted with remove before add"
            $ operations [placed 1 1] [placed 0 2]
            `shouldBe` [Remove $ placed 1 1, Add $ placed 0 2]
        it "have compressed remove and add on the same tool into move, forward"
            $ operations [placed 0 1] [placed 1 1]
            `shouldBe` [Move (Tool 1) 0 1]
        it "have compressed remove and add on the same tool into move, backward"
            $ operations [placed 1 1] [placed 0 1]
            `shouldBe` [Move (Tool 1) 1 0]
        it "have compressed remove and add on the same tool into move, middle"
            $ operations [placed 0 1, placed 2 1] [placed 1 1]
            `shouldBe` [Remove $ placed 2 1, Move (Tool 1) 0 1]
        it "have move operations safe after remove operations"
            $ operations
                [placed 0 1, placed 1 2]
                [placed 1 1]
            `shouldBe` [Remove $ placed 1 2, Move (Tool 1) 0 1]
