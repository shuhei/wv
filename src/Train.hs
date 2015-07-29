module Train ( train
             , buildMatrices
             , sigmoid
             , skipgrams
             ) where

import Huffman
import Vocabulary

import Debug.Trace (trace)
import Prelude hiding (lookup)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap, lookup)
import Numeric.LinearAlgebra.Data (Matrix, Vector, (><), (!), (?), toList, asRow, cmap, size)
import Numeric.LinearAlgebra.HMatrix (app, mul, fromList, scale, outer)
import Numeric.LinearAlgebra.Devel (runSTMatrix, thawMatrix, writeMatrix, modifyMatrix, mapVectorWithIndexM_, mapMatrixWithIndexM_)
import System.Random (RandomGen, randomRs)

-- | Weight matrix of input to hidden and hidden to output.
type WeightMatrix = (Matrix Double, Matrix Double)

-- TODO: Make it configurable.
vectorSize :: Int
vectorSize = 100

-- TODO: Bias node/row?
-- | Build a weight motrix with a model.
-- | - input to hidden: Voc x Vec
-- | - hidden to output: Vec x (Voc - 1)
buildMatrices :: RandomGen g => g -> Int -> WeightMatrix
buildMatrices g vocSize =
  let i2h = (vocSize >< vectorSize) [0, 0..]
      -- h2o is transposed.
      -- TODO: Use 'RandDist' to fill the initial weight matrices with random values.
      bound = 0.5 / fromIntegral vocSize
      h2o = ((vocSize - 1) >< vectorSize) $ randomRs (-bound, bound) g
  in (i2h, h2o)

-- | Train a weight matrix with sentences.
train :: Vocabulary
      -> HashMap Int Encoded
      -> WeightMatrix
      -> [Sentence]
      -> WeightMatrix
-- TODO: Make the learning rate configurable.
train voc huffman = foldl' $ trainSentence 0.025 voc huffman

-- | Train a weight matrix with a sentence.
trainSentence :: Double
              -> Vocabulary
              -> HashMap Int Encoded
              -> WeightMatrix
              -> Sentence
              -> WeightMatrix
trainSentence rate voc huffman matrices sentence =
  let indices = map (fst . fromJust . (`lookup` voc)) sentence
  in foldl' (trainSkipgram huffman rate) matrices $ skipgrams 5 indices

trainSkipgram :: HashMap Int Encoded
              -> Double
              -> WeightMatrix
              -> (Int, [Int])
              -> WeightMatrix
trainSkipgram huffman rate matrices (inputIndex, outputIndices) =
  foldl' (trainPair huffman rate inputIndex) matrices outputIndices

trainPair :: HashMap Int Encoded -- Huffman coding.
          -> Double -- Learning rate.
          -> Int -- Input word's index in the input layer.
          -> WeightMatrix -- Old matrices.
          -> Int -- Output word's index in the output layer.
          -> WeightMatrix
trainPair huffman rate inputIndex (i2h, h2o) outputIndex =
  -- HACK: Using `lookup` because `!` is reserved for `Matrix`.
  let encoded = fromJust $ lookup outputIndex huffman
      -- TODO: No activate function for the hidden layer?
      -- Vector: vectorSize
      hidden = i2h ! inputIndex
      -- Matrix: (L(w) - 1 >< vectorSize)
      wo = h2o ? point encoded
      -- Vector: L(w) - 1
      idealOutputs = fromList $ map direction $ code encoded
      -- Vector: L(w) - 1
      errors = cmap sigmoid (app wo hidden) - idealOutputs
      -- Update hidden to output matrix.
      -- TODO: Is `transpose $ outer hidden errors` same as `outer errors hidden`?
      deltaH2O = - rate `scale` (errors `outer` hidden)
      -- h2o is transposed.
      newH2O = updateRows h2o deltaH2O $ point encoded
      -- Update input to hidden matrix.
      deltaI2H = - rate `scale` (asRow errors `mul` wo)
      newI2H = updateRows i2h deltaI2H [inputIndex]
  in (newI2H, newH2O)

updateRows :: Matrix Double -> Matrix Double -> [Int] -> Matrix Double
updateRows mat delta rowIndices = runSTMatrix $ do
  {- m <- trace ("delta size: " ++ show (size delta) ++ " mat size: " ++ show (size mat)) $ thawMatrix mat -}
  m <- thawMatrix mat
  mapMatrixWithIndexM_ (\(i, j) d -> modifyMatrix m  (rowIndices !! i) j (+ d)) delta
  return m

-- | Sigmoid function.
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

-- TODO: Randomize the window size.
-- | Return skipgram pairs from a list.
skipgrams :: Int -> [a] -> [(a, [a])]
skipgrams size = skipgrams' size []
  -- `prev` is a reversed list of previous items.
  where skipgrams' size prev [] = []
        skipgrams' size prev (x:xs) =
          (x, reverse (take size prev) ++ take size xs) : skipgrams' size (x:prev) xs
