import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
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

    it "rejects any rolls greater than 10" $ do
      score [11,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1] `shouldBe` Nothing

    it "rejects any rolls less than 0" $ do
      score [-1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1] `shouldBe` Nothing

    it "rejects any frame with more than 10 total points" $ do
      score [-1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1] `shouldBe` Nothing

  describe "Construct Game" $ do
    it "assembles a normal frame" $ do
      constructGame [1,1] 1 `shouldBe` (oneFrameGame $ lastFram 1 1)

    it "assembles a strike frame" $ do
      constructGame [10, 1, 1] 2 `shouldBe` (buildGame [Strike] $ lastFram 1 1)

    it "assembles a spare frame" $ do
      constructGame [9, 1, 10, 1, 1] 3`shouldBe` (buildGame [Spare 9 1, Strike] $ lastFram 1 1)

    it "rejects frames with a value greater than 10" $ do
      constructGame [9, 2] 1 `shouldBe` Nothing

    it "rejects frames with an odd number of normal rolls" $ do
      constructGame [1, 1, 1] 1 `shouldBe` Nothing

    it "assembles frames finished by a strike" $ do
      constructGame [1, 1, 10, 1, 1 ] 2 `shouldBe` (buildGame [NormalFrame 1 1] $ lastFrame 10 1 1)