module TrainSpec where

import Test.Hspec
import Test.Hspec.Expectations
import Train

shouldBeBetween :: Double -> (Double, Double) -> Expectation
shouldBeBetween x (min, max) =
  x `shouldSatisfy` \x -> min <= x && x <= max

spec :: Spec
spec = do
  describe "sigmoid" $ do
    it "is 1/2 for 0" $
      sigmoid 0 `shouldBe` 0.5

    it "calculates sigmoid function" $
      sigmoid 1 `shouldBeBetween` (0.73105, 0.73106)

    it "fits in range from 0 to 1" $ do
      sigmoid 10000 `shouldBeBetween` (0.9999, 1)
      sigmoid (-10000) `shouldBeBetween` (0, 0.0001)

  describe "skipgrams" $ do
    it "returns skipgrams" $
      skipgrams 2 [1..5] `shouldBe` [(1, [2, 3]), (2, [1, 3, 4]), (3, [1, 2, 4, 5]), (4, [2, 3, 5]), (5, [3, 4])]

    it "works with an infinite list" $
      take 3 (skipgrams 2 [0..]) `shouldBe` [(0, [1, 2]), (1, [0, 2, 3]), (2, [0, 1, 3, 4])]
