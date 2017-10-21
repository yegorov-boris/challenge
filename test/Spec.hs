import Test.Hspec

import Lib

main = hspec $ do
  describe "Lib Tests" $ do
--     describe "someFunc"
--       it "should multiply ints" $ someFunc 7 7 `shouldBe` (49 :: Int)
    describe "likes" $ do
      it "empty" $ likes [] `shouldBe` "no one likes this"
      it "1 name" $ likes ["Peter"] `shouldBe` "Peter likes this"
      it "2 names" $ likes ["Jacob", "Alex"] `shouldBe` "Jacob and Alex like this"
      it "3 names" $ likes ["Max", "John", "Mark"] `shouldBe` "Max, John and Mark like this"
      it "4 names" $ likes ["Alex", "Jacob", "Mark", "Max"] `shouldBe` "Alex, Jacob and 2 others like this"
