module Homework1Test where

import Test.Hspec
import Homework1

main :: IO ()
main = hspec $ do
  describe "toDigits" $ do
    it "converts positive numbers to digits" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]
      toDigits 0 `shouldBe` [0]
    it "returns empty list for negative numbers" $ do
      toDigits (-17) `shouldBe` []

  describe "doubleEveryOther" $ do
    it "doubles every other digit from the right" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

  describe "sumDigits" $ do
    it "sums all digits in a list" $ do
      sumDigits [16,7,12,5] `shouldBe` 22
      sumDigits [1,2,3] `shouldBe` 6

  describe "validate" $ do
    it "validates correct credit card numbers" $ do
      validate 4012888888881881 `shouldBe` True
    it "rejects incorrect credit card numbers" $ do
      validate 4012888888881882 `shouldBe` False 