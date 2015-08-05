module HuffmanSpec where

import Test.Hspec
import Huffman
import qualified Data.IntMap.Strict as IM

sampleTree :: Tree Int
sampleTree = Branch (Branch (Branch (Leaf 1 2) (Leaf 0 3) 0 5) (Leaf 3 5) 1 10) (Leaf 2 11) 2 21

spec :: Spec
spec = do
  describe "weight" $ do
    it "returns weight of a leaf" $
      weight (Leaf 2 3) `shouldBe` 3

    it "returns weight of a branch" $
      weight (Branch (Leaf 0 0) (Leaf 1 0) 0 7) `shouldBe` 7

  describe "merge" $
    it "sums weights of two leaves" $ do
      let merged = merge 0 (Leaf 1 4) (Leaf 2 7)
      weight merged `shouldBe` 11

  describe "buildTree" $
    it "builds a tree from a list of something and its weight" $ do
      let list = [(0, 3), (1, 2), (2, 11), (3, 5)]
      buildTree list `shouldBe` sampleTree

  describe "encodeTree" $
    it "encodes a tree into an int map" $ do
      let encoded = encodeTree sampleTree
      IM.keys encoded `shouldBe` [0, 1, 2, 3]
      encoded IM.! 0 `shouldBe` Encoded { code = [L, L, R], point = [2, 1, 0] }
      encoded IM.! 1 `shouldBe` Encoded { code = [L, L, L], point = [2, 1, 0] }
      encoded IM.! 2 `shouldBe` Encoded { code = [R],       point = [2] }
      encoded IM.! 3 `shouldBe` Encoded { code = [L, R],    point = [2, 1] }
