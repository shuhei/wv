{-# LANGUAGE NamedFieldPuns, RecordWildCards, RankNTypes #-}

module Train ( Configuration(..)
             , train
             , buildMatrices
             , sigmoid
             , skipgrams
             ) where

import Huffman
import Vocabulary

import Prelude hiding (lookup)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Arrow ((***))
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap, lookup)
import Foreign.Storable (Storable)
import Numeric.LinearAlgebra.Data (Matrix, Vector, (><), (!), (?), toList, asRow, cmap, size)
import Numeric.LinearAlgebra.HMatrix (app, mul, fromList, scale, outer)
import Numeric.LinearAlgebra.Devel (STMatrix (..), runSTMatrix, thawMatrix, writeMatrix, modifyMatrix, mapVectorWithIndexM_, mapMatrixWithIndexM_, unsafeFreezeMatrix, freezeMatrix)
import System.Random (RandomGen, randomRs)

data Configuration = Configuration
                   { vocabulary :: Vocabulary
                   , huffman :: HashMap Int Encoded
                   , learningRate :: Double
                   , vectorSize :: Int
                   } deriving (Show)

-- | Weight matrix of input to hidden and hidden to output.
type WeightMatrix = (Matrix Double, Matrix Double)

-- TODO: Bias node/row?
-- | Build a weight motrix with a model.
-- | - input to hidden: Voc x Vec
-- | - hidden to output: Vec x (Voc - 1)
buildMatrices :: RandomGen g => g -> Int -> Int -> WeightMatrix
buildMatrices g vocSize vecSize =
  let i2h = (vocSize >< vecSize) [0, 0..]
      -- h2o is transposed.
      bound = 0.5 / fromIntegral vocSize
      h2o = ((vocSize - 1) >< vecSize) $ randomRs (-bound, bound) g
  in (i2h, h2o)

-- | Train a weight matrix with sentences.
train :: Configuration
      -> WeightMatrix
      -> [Sentence]
      -> WeightMatrix
train config (i2h, h2o) sentences = runSTMatrices $ do
  si2h <- thawMatrix i2h
  sh2o <- thawMatrix h2o
  mapM_ (trainSentence config (si2h, sh2o)) sentences
  return (si2h, sh2o)

-- | Train a weight matrix with a sentence.
trainSentence :: Configuration
              -> (STMatrix s Double, STMatrix s Double)
              -> Sentence
              -> ST s ()
trainSentence config@(Configuration {vocabulary, ..}) matrices sentence = do
  let indices = map (fst . fromJust . (`lookup` vocabulary)) sentence
  mapM_ (trainSkipgram config matrices) $ skipgrams 5 indices

trainSkipgram :: Configuration
              -> (STMatrix s Double, STMatrix s Double)
              -> (Int, [Int])
              -> ST s ()
trainSkipgram config matrices (inputIndex, outputIndices) =
  mapM_ (trainPair config matrices inputIndex) outputIndices

trainPair :: Configuration  -- ^ Configuration for training.
          -> (STMatrix s Double, STMatrix s Double) -- ^ Old matrices.
          -> Int            -- ^ Input word's index in the input layer.
          -> Int            -- ^ Output word's index in the output layer.
          -> ST s ()
trainPair Configuration {huffman, learningRate, ..} (si2h, sh2o) inputIndex outputIndex = do
  -- HACK: Using `lookup` because `!` is reserved for `Matrix`.
  let encoded = fromJust $ lookup outputIndex huffman
  -- TODO: Guessing `unsafeFreezeMatrix` doesn't create a new matrix...
  i2h <- unsafeFreezeMatrix si2h
  h2o <- unsafeFreezeMatrix sh2o
      -- TODO: No activate function for the hidden layer?
      -- Vector: vectorSize
  let hidden = i2h ! inputIndex
      -- TODO: This seems to be slow because it's O(vocSize) and copies a new matrix.
      -- TODO: Create `unsafe?` or something.
      -- Matrix: (L(w) - 1 >< vectorSize)
      wo = h2o ? point encoded
      -- Vector: L(w) - 1
      idealOutputs = fromList $ map direction $ code encoded
      -- Vector: L(w) - 1
      errors = cmap sigmoid (app wo hidden) - idealOutputs
      -- Calculate deltas to update weight matrices.
  -- TODO: Does separated `let` in `do` strictly calculate the result?
  let deltaH2O = - learningRate `scale` (errors `outer` hidden)
  let deltaI2H = - learningRate `scale` (asRow errors `mul` wo)

  updateRows sh2o deltaH2O $ point encoded
  updateRows si2h deltaI2H [inputIndex]

updateRows :: STMatrix s Double
           -> Matrix Double
           -> [Int]
           -> ST s ()
updateRows mat delta rowIndices = mapMatrixWithIndexM_ modify delta
  where modify (i, j) d = modifyMatrix mat  (rowIndices !! i) j (+ d)

-- | Sigmoid function.
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

-- TODO: Randomize the window size (1..size).
-- | Return skipgram pairs from a list.
skipgrams :: Int -> [a] -> [(a, [a])]
skipgrams size = skipgrams' size []
  -- `prev` is a reversed list of previous items.
  where skipgrams' size prev [] = []
        skipgrams' size prev (x:xs) =
          (x, reverse (take size prev) ++ take size xs) : skipgrams' size (x:prev) xs

runSTMatrices :: (Storable t1, Storable t2)
              => (forall s. ST s (STMatrix s t1, STMatrix s t2))
              -> (Matrix t1, Matrix t2)
runSTMatrices st = runST $ do
  (sm1, sm2) <- st
  m1 <- unsafeFreezeMatrix sm1
  m2 <- unsafeFreezeMatrix sm2
  return (m1, m2)
