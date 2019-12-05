-- file test/Day05Spec.hs
module Day05Spec (spec) where

import Test.Hspec
import Day05

sample = parseInput "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\
                    \ 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\
                    \ 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

x `shouldOutput` y = (head $ fst x) `shouldBe` y

spec = do
  describe "sample program" $ do
    it "outputs 999 for inputs below 8" $ do
      execute [-4] sample `shouldOutput` 999

    it "outputs 1000 for input equal to 8" $ do
      execute [8] sample `shouldOutput` 1000

    it "outputs 1001 for inputs above 8" $ do
      execute [1911287] sample `shouldOutput` 1001
