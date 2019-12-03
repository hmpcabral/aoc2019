-- file test/Day03Spec.hs
module Day03Spec (spec) where

import Test.Hspec
import Day03

closest ws = part1 . parseInput $ unlines ws
fastest ws = part2 . parseInput $ unlines ws

spec = do
  describe "part1" $ do
    it "sample wires" $ do
      closest ["R8,U5,L5,D3", "U7,R6,D4,L4"] `shouldBe` 6
      closest [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"
              , "U62,R66,U55,R34,D71,R55,D58,R83"] `shouldBe` 159
      closest [ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
              , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ] `shouldBe` 135

  describe "part2" $ do
    it "sample wires" $ do
      fastest ["R8,U5,L5,D3", "U7,R6,D4,L4"] `shouldBe` 30
      fastest [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"
              , "U62,R66,U55,R34,D71,R55,D58,R83"] `shouldBe` 610
      fastest [ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
              , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ] `shouldBe` 410
