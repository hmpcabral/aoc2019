-- file test/Day06Spec.hs
module Day06Spec (spec) where

import Test.Hspec
import Day06

spec = do
  describe "part1" $ do
    it "sample orbits" $ do
      part1 (parseInput $ unlines [ "COM)B"
                                  , "B)C"
                                  , "C)D"
                                  , "D)E"
                                  , "E)F"
                                  , "B)G"
                                  , "G)H"
                                  , "D)I"
                                  , "E)J"
                                  , "J)K"
                                  , "K)L" ]) `shouldBe` 42

  describe "part2" $ do
    it "sample orbits" $ do
      part2 (parseInput $ unlines [ "COM)B"
                                  , "B)C"
                                  , "C)D"
                                  , "D)E"
                                  , "E)F"
                                  , "B)G"
                                  , "G)H"
                                  , "D)I"
                                  , "E)J"
                                  , "J)K"
                                  , "K)L"
                                  , "K)YOU"
                                  , "I)SAN" ]) `shouldBe` 4
