-- file test/Day05Spec.hs
module Day05Spec (spec) where

import Test.Hspec
import Day05

given :: [Int] -> String -> [Int]
given inp s = fst . execute inp $ parseInput s

running :: String -> [Int]
running = given []

x `shouldOutput` y = head x `shouldBe` y

spec = do
  describe "individual opcodes" $ do
    it "adds position operands" $ do
      running "1,7,8,9,4,9,99,3,7,0" `shouldOutput` 10

    it "adds immediate operands" $ do
      running "101,7,8,9,4,9,99,3,7,0" `shouldOutput` 14

    it "multiplies position operands" $ do
      running "2,7,8,9,4,9,99,3,7,0" `shouldOutput` 21

    it "multiplies immediate operands" $ do
      running "102,7,8,9,4,9,99,3,7,0" `shouldOutput` 49

    it "position output" $ do
      running "4,3,99,23" `shouldOutput` 23

    it "immediate output" $ do
      running "104,23,99" `shouldOutput` 23

    it "input" $ do
      given [42] "3,5,4,5,99,0" `shouldOutput` 42

    it "jumps if true with position operand" $ do
      running "1005,9,6,104,10,99,104,20,99,0" `shouldOutput` 10
      running "1005,9,6,104,10,99,104,20,99,1" `shouldOutput` 20

    it "jumps if true with immediate operand" $ do
      running "1105,0,6,104,10,99,104,20,99" `shouldOutput` 10
      running "1105,1,6,104,10,99,104,20,99" `shouldOutput` 20

    it "less than with position operands" $ do
      running "7,7,8,9,4,9,99,3,7,0" `shouldOutput` 1
      running "7,7,8,9,4,9,99,7,3,0" `shouldOutput` 0

    it "less than with immediate operands" $ do
      running "107,7,8,9,4,9,99,7,3,0" `shouldOutput` 0
      running "107,7,8,9,4,9,99,7,8,0" `shouldOutput` 1

    it "equals with position operands" $ do
      running "8,7,8,9,4,9,99,3,3,0" `shouldOutput` 1
      running "8,7,8,9,4,9,99,3,7,0" `shouldOutput` 0

    it "equals with immediate operands" $ do
      running "108,7,8,9,4,9,99,7,3,0" `shouldOutput` 0
      running "108,7,8,9,4,9,99,7,7,0" `shouldOutput` 1

  describe "sample program" $ do
    let sample = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\
                 \ 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\
                 \ 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

    it "outputs 999 for inputs below 8" $ do
      given [-4] sample `shouldOutput` 999

    it "outputs 1000 for input equal to 8" $ do
      given [8] sample `shouldOutput` 1000

    it "outputs 1001 for inputs above 8" $ do
      given [1911287] sample `shouldOutput` 1001
