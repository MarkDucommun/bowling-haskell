module RequestParserSpec (spec) where

import Test.Hspec
import RequestParser

spec :: Spec
spec = do
  describe "Request Parser" $ do
    describe "GET" $ do
      it "Parses a minimally viable request" $ do
        parseRequest ["GET / HTTP/1.1"] `shouldBe`  (Just $ GET "/" Nothing)

      it "Parses a request with one param" $ do
        parseRequest ["GET /foo?a=bar HTTP/1.1"] `shouldBe` (Just $ GET "/foo" $ Just [("a", "bar")])

    describe "POST" $ do
      it "Parses a minimally viable request" $ do
        parseRequest ["POST / HTTP/1.1\r", "Content-Length: 4\r","\r", "BODY"] `shouldBe`  (Just $ POST "/" $ Just "BODY")

    it "Fails to parse a request without a route" $ do
      parseRequest ["GET HTTP/1.1"] `shouldBe` Nothing