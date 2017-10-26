import Test.Hspec
import Test.QuickCheck

import Lib

main = hspec $ do
  describe "Lib Tests" $ do
--     describe "someFunc"
--       it "should multiply ints" $ someFunc 7 7 `shouldBe` (49 :: Int)
--     describe "likes" $ do
--       it "empty" $ likes [] `shouldBe` "no one likes this"
--       it "1 name" $ likes ["Peter"] `shouldBe` "Peter likes this"
--       it "2 names" $ likes ["Jacob", "Alex"] `shouldBe` "Jacob and Alex like this"
--       it "3 names" $ likes ["Max", "John", "Mark"] `shouldBe` "Max, John and Mark like this"
--       it "4 names" $ likes ["Alex", "Jacob", "Mark", "Max"] `shouldBe` "Alex, Jacob and 2 others like this"
--     describe "findMissing" $ do
--     it "should work for some simple tests" $ do
--       findMissing [1,2,3,4,6] `shouldBe` 5
--       findMissing [1,3,5,9]   `shouldBe` 7
--       findMissing [1,2,4,5]   `shouldBe` 3
--       findMissing [1,2,4]     `shouldBe` 3

--       it "should work for some random tests" $ do
--         property             $ \x ->      -- start value
--           forAll nonzero     $ \y ->      -- offset
--           forAll positive    $ \n ->      -- missing index
--           forAll (greater 3) $ \z ->      -- sequence length
--           let xs' = take z [x,x+y..]      -- an arithmetic sequence
--               n'  = min (z - 2) n         -- an index guaranteed to be in that sequence
--               x'  = x + y * n'            -- the according number in the sequence
--               xs  = filter (/= x') $ xs'  -- the sequence without that number
--           in findMissing xs `shouldBe` x' -- the test against your function

--       it "should work for some random tests with infinite lists" $ do
--         property             $ \x ->      -- start value
--           forAll nonzero     $ \y ->      -- offset
--           forAll positive    $ \n ->      -- missing index
--           let xs' = [x,x+y..]             -- an arithmetic sequence
--               x'  = x + y * n :: Integer  -- the according number in the sequence
--               xs  = filter (/= x') $ xs'  -- the sequence without that number
--           in findMissing xs `shouldBe` x' -- the test against your function
    describe "isPrime" $ do
      it "should work for some examples" $ do
        isPrime 0        `shouldBe` False
        isPrime 1        `shouldBe` False
        isPrime 2        `shouldBe` True
        isPrime 17       `shouldBe` True
        isPrime 23423527 `shouldBe` True
--       it "should work for negative numbers" $ quickCheckWith stdArgs { maxSuccess = 100 } $ do
--         forAll (choose (-1,-(2^32))) $ \x -> do
--           isPrime x `shouldBe` False
--   where greater :: (Ord n, Arbitrary n) => n -> Gen n
--         greater n = arbitrary `suchThat` (>n)
--         positive, nonzero :: (Num n, Eq n, Ord n, Arbitrary n) => Gen n
--         positive  = greater 0
--         nonzero   = arbitrary `suchThat` (/=0)
