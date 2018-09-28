module InputSpec(spec) where

import Test.Hspec
import InputLib

spec :: Spec
spec = do
  describe "Parse Input" $ do
    it "converts a really simple string to an array of an Int" $ do
      parseInput "1" `shouldBe` Just [1]

    it "converts a slightly more complex string to an array of an Int" $ do
      parseInput "1 2" `shouldBe` Just [1, 2]

  describe "String to Int" $ do
    it "converts a very simple string to an int" $ do
      parseString "1" `shouldBe` Just 1

    it "converts a longer number to an int" $ do
      parseString "10" `shouldBe` Just 10

    it "fails converting numbers greater than " $ do
      parseString "111" `shouldBe` Just 111

  describe "Split on spaces" $ do
    it "splits on spaces" $ do
      splitOnSpaces "123 123" `shouldBe` ["123", "123"]