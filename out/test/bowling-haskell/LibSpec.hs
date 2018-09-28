module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "Bowling" $ do
    it "scores a simple game" $ do
      score [1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1] `shouldBe` Just 20

    it "scores a game with a strike" $ do
      score [10, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1] `shouldBe` Just 30

    it "scores a game with all strikes" $ do
      score [10, 10, 10, 10, 10, 10, 10, 10, 10, 10,10,10] `shouldBe` Just 300

    it "scores a game with all spares" $ do
      score [9,1, 9,1, 9,1, 9,1, 9,1, 9,1, 9,1, 9,1, 9,1, 9,1,9] `shouldBe` Just 190

    it "scores a game with all gutters" $ do
      score [0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0] `shouldBe` Just 0

    it "rejects a game without 10 valid frames" $ do
      score [0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0] `shouldBe` Nothing

    it "rejects any rolls greater than 10" $ do
      score [11,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1] `shouldBe` Nothing

    it "rejects any rolls less than 0" $ do
      score [-1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1] `shouldBe` Nothing

    it "rejects any frame with more than 10 total points" $ do
      score [9,2, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1] `shouldBe` Nothing
