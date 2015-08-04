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
import qualified Data.HashMap.Strict as H (HashMap, empty, insert, lookup)

-- | A sentence is a list of words.
type Sentence = [String]

-- | Index and weight.
type IndexWeight = (Int, Int)

type Vocabulary = H.HashMap String IndexWeight

type Index2Word = H.HashMap Int String

-- TODO: Too abstract name. Rename properly.
-- | Vocabulary, index to word and current vocabulary size.
type Model = (Vocabulary, Index2Word, Int)

-- | Build a model from sentences.
buildModel :: [Sentence] -> Model
buildModel = foldl' addWords (H.empty, H.empty, 0)

-- Add words to a model.
addWords :: Model -> [String] -> Model
addWords = foldl' addWord

-- Add a word to a model.
-- O(log(voc size))
addWord :: Model -> String -> Model
addWord (voc, i2w, n) word = case H.lookup word voc of
  Nothing       -> (H.insert word (n, 1) voc, H.insert n word i2w, n + 1)
  Just (idx, c) -> (H.insert word (idx, c + 1) voc, i2w, n)
