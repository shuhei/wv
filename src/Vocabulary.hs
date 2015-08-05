module Vocabulary
    ( Sentence
    , IndexWeight
    , Vocabulary
    , Index2Word
    , Model
    , buildModel
    ) where

import Debug.Trace (trace)
import Data.List (foldl')
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM

-- | A sentence is a list of words.
type Sentence = [String]

-- | Index and weight.
type IndexWeight = (Int, Int)

type Vocabulary = HM.HashMap String IndexWeight

type Index2Word = IM.IntMap String

-- TODO: Too abstract name. Rename properly.
-- | Vocabulary, index to word and current vocabulary size.
type Model = (Vocabulary, Index2Word, Int)

-- | Build a model from sentences.
buildModel :: [Sentence] -> Model
buildModel = foldl' addWords (HM.empty, IM.empty, 0)

-- Add words to a model.
addWords :: Model -> [String] -> Model
addWords = foldl' addWord

-- Add a word to a model.
-- O(log(voc size))
addWord :: Model -> String -> Model
addWord (voc, i2w, n) word = case HM.lookup word voc of
  Nothing       -> (HM.insert word (n, 1) voc, IM.insert n word i2w, n + 1)
  Just (idx, c) -> (HM.insert word (idx, c + 1) voc, i2w, n)
