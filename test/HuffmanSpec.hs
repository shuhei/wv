module HuffmanSpec where

import Test.Hspec
import Huffman
import Data.HashMap.Strict ((!), keys)

sampleTree :: Tree Char
sampleTree = Branch (Branch (Branch (Leaf 'b' 2) (Leaf 'a' 3) 0 5) (Leaf 'd' 5) 1 10) (Leaf 'c' 11) 2 21

spec :: Spec
spec = do
  describe "weight" $ do
    it "returns weight of a leaf" $ do
      weight (Leaf 'c' 3) `shouldBe` 3

    it "returns weight of a branch" $ do
      weight (Branch (Leaf 'a' 0) (Leaf 'b' 0) 0 7) `shouldBe` 7

  describe "merge" $ do
    it "sums weights of two leaves" $ do
      merged <- return $ merge 0 (Leaf '1' 4) (Leaf '2' 7)
      weight merged `shouldBe` 11

  describe "buildTree" $ do
    it "builds a tree from a list of something and its weight" $ do
      list <- return [('a', 3), ('b', 2), ('c', 11), ('d', 5)]
      buildTree list `shouldBe` sampleTree

  describe "encodeTree" $ do
    it "encodes a tree into a hash map" $ do
      encoded <- return $ encodeTree sampleTree
      keys encoded `shouldBe` ['a', 'b', 'c', 'd']
      encoded ! 'a' `shouldBe` Encoded { code = [L, L, R], point = [2, 1, 0] }
      encoded ! 'b' `shouldBe` Encoded { code = [L, L, L], point = [2, 1, 0] }
      encoded ! 'c' `shouldBe` Encoded { code = [R],       point = [2] }
      encoded ! 'd' `shouldBe` Encoded { code = [L, R],    point = [2, 1] }
