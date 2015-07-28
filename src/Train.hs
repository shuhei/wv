module Train ( train
             , buildMatrices
             ) where

import Huffman
import Vocabulary

import Prelude hiding (lookup)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap, lookup)
import Numeric.LinearAlgebra.Data (Matrix, Vector, (><), (!), (?), toList, cmap)
import Numeric.LinearAlgebra.HMatrix (app, fromList, scale, outer)
import Numeric.LinearAlgebra.Devel (runSTMatrix, thawMatrix, writeMatrix, modifyMatrix, mapVectorWithIndexM_, mapMatrixWithIndexM_)

-- | Weight matrix of input to hidden and hidden to output.
type WeightMatrix = (Matrix Double, Matrix Double)

-- TODO: Make it configurable.
vectorSize :: Int
vectorSize = 100

-- TODO: Bias node/row?
-- | Build a weight motrix with a model.
-- | - input to hidden: Voc x Vec
-- | - hidden to output: Vec x (Voc - 1)
buildMatrices :: Int -> WeightMatrix
buildMatrices vocSize =
  let i2h = (vocSize >< vectorSize) [0, 0..]
      -- TODO: Use 'RandDist' to fill the initial weight matrices with random values.
      h2o = (vectorSize >< (vocSize - 1)) [0, 0..]
  in (i2h, h2o)

-- | Train a weight matrix with sentences.
train :: Vocabulary -> WeightMatrix -> [Sentence] -> WeightMatrix
train voc = foldl' $ trainSentence voc

-- TODO: Implement.
-- | Train a weight matrix with a sentence.
trainSentence :: Vocabulary -> WeightMatrix -> Sentence -> WeightMatrix
trainSentence = undefined

-- TODO: Change the argument order as needed.
trainPair :: Int -- Input word's index in the input layer.
          -> Int -- Output word's index in the output layer.
          -> Vocabulary -- ???
          -> HashMap Int Encoded -- Huffman coding.
          -> Double -- Learning rate.
          -> WeightMatrix -- Old matrices.
          -> WeightMatrix
trainPair inputIndex outputIndex voc huffman rate (i2h, h2o) =
  -- HACK: Using `lookup` because `!` is reserved for `Matrix`.
  let encoded = fromJust $ lookup outputIndex huffman
      -- TODO: No activate function for the hidden layer?
      hidden = i2h ! inputIndex
      wo = h2o ? point encoded
      idealOutputs = fromList $ map direction $ code encoded
      errors = cmap sigmoid (app wo hidden) - idealOutputs
      -- Update hidden to output matrix.
      deltaH2O = - rate `scale` (hidden `outer` errors)
      newH2O = updateRows h2o deltaH2O $ point encoded
      -- Update input to hidden matrix.
      -- TODO: Check whether `app` is correct or not.
      deltaI2H = - rate `scale` (wo `app` errors)
      newI2H = updateRow i2h deltaI2H inputIndex
  in (newI2H, newH2O)

updateRows :: Matrix Double -> Matrix Double -> [Int] -> Matrix Double
updateRows mat delta rowIndices = runSTMatrix $ do
  m <- thawMatrix mat
  mapMatrixWithIndexM_ (\(i, j) d -> modifyMatrix m  (rowIndices !! i) j (+ d)) delta
  return m

updateRow :: Matrix Double -> Vector Double -> Int -> Matrix Double
updateRow mat delta index = runSTMatrix $ do
  m <- thawMatrix mat
  mapVectorWithIndexM_ (\j d -> modifyMatrix m index j (+ d)) delta
  return m

-- | Sigmoid function.
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))
