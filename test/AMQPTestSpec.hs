module RMQDriverSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "foo" $ do
    it "is 1" $ do
      1 `shouldBe` 1
