-- file test/Day04Spec.hs
module Day04Spec (spec) where

import Test.Hspec
import Day04

f `shouldAccept` n = f (n, n) `shouldBe` 1
f `shouldReject` n = f (n, n) `shouldBe` 0

spec = do
  describe "part1" $ do
    it "accepts a single double" $ do
      part1 `shouldAccept` 122345
      part1 `shouldAccept` 112345
      part1 `shouldAccept` 123455

    it "accepts multiple doubles" $ do
      part1 `shouldAccept` 112233

    it "accepts larger groups" $ do
      part1 `shouldAccept` 111234
      part1 `shouldAccept` 122234
      part1 `shouldAccept` 123444

    it "rejects decreasing sequences" $ do
      part1 `shouldReject` 213456
      part1 `shouldReject` 130456
      part1 `shouldReject` 123465

    it "rejects non-doubles" $ do
      part1 `shouldReject` 123456
      part1 `shouldReject` 121456

  describe "part2" $ do
    it "accepts a single double" $ do
      part2 `shouldAccept` 122345
      part2 `shouldAccept` 112345
      part2 `shouldAccept` 123455

    it "accepts multiple doubles" $ do
      part2 `shouldAccept` 112233

    it "rejects larger groups" $ do
      part2 `shouldReject` 111234
      part2 `shouldReject` 122234
      part2 `shouldReject` 123444

    it "accepts a double with larger group" $ do
      part2 `shouldAccept` 111223

    it "rejects decreasing sequences" $ do
      part2 `shouldReject` 213456
      part2 `shouldReject` 130456
      part2 `shouldReject` 123465

    it "rejects non-doubles" $ do
      part2 `shouldReject` 123456
      part2 `shouldReject` 121456
