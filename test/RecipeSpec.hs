module RecipeSpec where

import Recipe
    ( ErrorRecipe (InvalidWidth, OverlappingPlaceds)
    , firstFit
    , insert
    , validSequence
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Tool (Placed (Placed), Tool (..), placed)

spec :: Spec
spec = do
    describe "validity passes for" $ do
        it "empty recipe"
            $ validSequence 0 []
            `shouldBe` Right ()
        it "recipe with one tool inside width"
            $ validSequence 1 [placed 0 1]
            `shouldBe` Right ()
    describe "validity fails for" $ do
        it "recipe with one too big tool"
            $ validSequence 1 [placed 0 2]
            `shouldBe` Left (InvalidWidth $ placed 0 2)
        it "recipe with tool placed after width"
            $ validSequence 1 [placed 1 1]
            `shouldBe` Left (InvalidWidth $ placed 1 1)
        it "recipe with overlapping tools"
            $ validSequence 2 [placed 0 1, placed 1 1]
            `shouldBe` Left (OverlappingPlaceds (placed 0 1) (placed 1 1))
    describe "insert" $ do
        it "inserts a tool at the beginning"
            $ insert (placed 0 1) []
            `shouldBe` [placed 0 1]
        it "inserts a tool at the end"
            $ insert (placed 1 1) [placed 0 1]
            `shouldBe` [placed 0 1, placed 1 1]
        it "inserts a tool in the middle"
            $ insert (placed 1 1) [placed 0 1, placed 2 1]
            `shouldBe` [placed 0 1, placed 1 1, placed 2 1]
    describe "firstSpace" $ do
        let firstFit = Recipe.firstFit . Tool
        it "finds the first space in an empty recipe"
            $ firstFit 1 []
            `shouldBe` 0
        it "finds the first space in a recipe with one tool"
            $ firstFit 1 [placed 0 1]
            `shouldBe` 1
        it "finds the first space in a recipe with two tools, middle"
            $ firstFit 1 [placed 0 1, placed 2 1]
            `shouldBe` 1
        it "finds the first space in a recipe with three tools, middle"
            $ firstFit 1 [placed 0 1, placed 1 1, placed 3 1]
            `shouldBe` 2
