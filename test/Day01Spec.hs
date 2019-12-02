-- file test/Day01Spec.hs
module Day01Spec (spec) where

import Test.Hspec
import Day01

spec = do
  describe "part1" $ do
    it "mass is multiple of 3" $ do
      part1 [12] `shouldBe` 2

    it "mass is not multiple of 3" $ do
      part1 [14] `shouldBe` 2

    it "various masses" $ do
      part1 [1969] `shouldBe` 654
      part1 [100756] `shouldBe` 33583

  describe "part2" $ do
    it "no extra fuel" $ do
      part2 [14] `shouldBe` 2

    it "various masses" $ do
      part2 [1969] `shouldBe` 966
      part2 [100756] `shouldBe` 50346
