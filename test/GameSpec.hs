module GameSpec (spec) where

import Test.Hspec
import GameLib

spec :: Spec
spec = do
  describe "Construct Game" $ do
    it "assembles a normal frame" $ do
      constructGame [1,1] 1 `shouldBe` Just ( Game Nothing $ LastFrame 1 1 Nothing)

    it "assembles a strike frame" $ do
      constructGame [10, 1, 1] 2 `shouldBe` Just (Game (Just [Strike]) $ LastFrame 1 1 Nothing)

    it "assembles a spare frame" $ do
      constructGame [9, 1, 10, 1, 1] 3`shouldBe` Just (Game (Just [Spare 9 1, Strike]) $ LastFrame 1 1 Nothing)

    it "rejects frames with a value greater than 10" $ do
      constructGame [9, 2] 1 `shouldBe` Nothing

    it "rejects frames with an odd number of normal rolls" $ do
      constructGame [1, 1, 1] 1 `shouldBe` Nothing

    it "assembles frames finished by a strike" $ do
      constructGame [1, 1, 10, 1, 1 ] 2 `shouldBe` Just (Game (Just [NormalFrame 1 1])  $ LastFrame 10 1 $ Just 1)