-- file test/Day02Spec.hs
module Day02Spec (spec) where

import Test.Hspec
import Day02

import Data.Vector.Unboxed (fromList)

p `shouldEndAt` q = execute (fromList p) `shouldBe` (fromList q)

spec = do
  describe "execute" $ do
    it "sample programs" $ do
      [1,0,0,0,99] `shouldEndAt` [2,0,0,0,99]
      [2,3,0,3,99] `shouldEndAt` [2,3,0,6,99]
      [2,4,4,5,99,0] `shouldEndAt` [2,4,4,5,99,9801]
      [1,1,1,4,99,5,6,0,99] `shouldEndAt` [30,1,1,4,2,5,6,0,99]
      [1,9,10,3,2,3,11,0,99,30,40,50] `shouldEndAt` [3500,9,10,70,2,3,11,0, 99,30,40,50]
