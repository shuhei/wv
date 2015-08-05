module Huffman where

import Data.List (insertBy, sortBy)
import Data.Ord (comparing)
import Data.Hashable
import qualified Data.IntMap.Strict as IM

-- | Left or Right.
data Code = L
          | R
          deriving (Show, Eq, Ord)

direction :: Code -> Double
direction L = 1
direction R = 0

-- | Code of Huffman coding and path from the root.
data Encoded = Encoded
             { code  :: [Code]
             , point :: [Int]
             } deriving (Show, Eq)

-- | A binary tree that is either a leaf or a branch.
data Tree a
  -- | A leaf contains any data and weight.
  = Leaf a Int
  -- | A branch contains left and right tree, an index and its weigth.
  | Branch (Tree a) (Tree a) Int Int
  deriving (Show, Eq, Ord)

-- | Get weight of a tree.
weight :: Tree a -> Int
weight (Leaf _ w)     = w
weight (Branch _ _ _ w) = w

-- | Get index of a branch. No index for a leaf.
index :: Tree a -> Int
index (Branch _ _ i _) = i

-- | Merge two trees with an index.
merge :: Int -> Tree a -> Tree a -> Tree a
merge i left right = Branch left right i (weight left + weight right)

-- | Build a binary tree from items.
-- TODO: Is this fast enough with `insertBy`? If not, try `Heap`.
buildTree :: [(a, Int)] -> Tree a
buildTree = build 0 . map (uncurry Leaf) . sortBy (comparing snd)
  where build _ ([t])      = t
        build i (t1:t2:ts) = build (i + 1) $ insertBy (comparing weight) (merge i t1 t2) ts

-- | Create an int map of word index as key and codes and points as value from a tree.
encodeTree :: Tree Int -> IM.IntMap Encoded
encodeTree tree = encode tree [] [] IM.empty

-- | Merge a tree into a hash map with codes and points.
-- TODO: Is `c ++ [L]` slow if `c` is huge? If yes, use `Sequence` instead of `List`.
-- Or code can be in reverse order.
encode :: Tree Int -> [Code] -> [Int] -> IM.IntMap Encoded -> IM.IntMap Encoded
encode (Leaf a _) c p hm               = IM.insert a Encoded { code = c, point = p } hm
encode (Branch left right i _) c p map = encode left (c ++ [L]) (p ++ [i]) map `IM.union` encode right (c ++ [R]) (p ++ [i]) IM.empty

-- | Calculate probability for a leaf and probabilities on its path.
softmax :: Encoded -> [Double] -> Double
softmax (Encoded codes _) ps = softmax' codes ps 1.0
  where softmax' [] [] prob = prob
        softmax' (L:cs) (p:ps) prob = softmax' cs ps $ prob * p
        softmax' (R:cs) (p:ps) prob = softmax' cs ps $ prob * (1.0 - p)
