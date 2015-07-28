module TrainSpec where

import Test.Hspec
import Test.Hspec.Expectations
import Train

shouldBeBetween :: Double -> (Double, Double) -> Expectation
shouldBeBetween x (min, max) =
  x `shouldSatisfy` \x -> min <= x && x <= max

spec :: Spec
spec =
  describe "sigmoid" $ do
    it "is 1/2 for 0" $
      sigmoid 0 `shouldBe` 0.5

    it "calculates sigmoid function" $
      sigmoid 1 `shouldBeBetween` (0.73105, 0.73106)

    it "fits in range from 0 to 1" $ do
      sigmoid 10000 `shouldBeBetween` (0.9999, 1)
      sigmoid (-10000) `shouldBeBetween` (0, 0.0001)
