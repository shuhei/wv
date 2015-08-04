module Word2VecSpec where

import Test.Hspec
import Test.Hspec.Expectations
import Word2Vec

spec :: Spec
spec =
  describe "groupOf" $
    it "splits a list into groups" $
      take 4 (groupOf 3 [0..]) `shouldBe` [[0, 1, 2], [3, 4, 5], [6, 7, 8], [9, 10, 11]]
