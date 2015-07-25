module Vocabulary
    ( Sentence
    , IndexWeight
    , Vocabulary
    , Index2Word
    , Model
    , buildModel
    ) where

import qualified Data.HashMap.Strict as H (HashMap, empty, insert, lookup)

-- TODO: Use `Vector` or other efficient data structure if necessary.

-- | A sentence is a list of words.
type Sentence = [String]

-- | Index and weight.
type IndexWeight = (Int, Int)

type Vocabulary = H.HashMap String IndexWeight

type Index2Word = H.HashMap Int String

-- TODO: Too abstract name. Rename properly.
-- | Vocabulary, index to word and current vocabulary size.
type Model = (Vocabulary, Index2Word, Int)

-- TODO: Can a `List` be concatenated lazily? If so, do it.
-- | Build a model from sentences.
buildModel :: [Sentence] -> Model
buildModel sentences = buildModel' sentences (H.empty, H.empty, 0)
  where buildModel' [] m     = m
        buildModel' (s:ss) m = buildModel' ss $ addWords s m

-- Add words to a model.
addWords :: [String] -> Model -> Model
addWords [] m     = m
addWords (w:ws) m = addWords ws $ addWord w m

-- Add a word to a model.
-- O(log(voc size))
addWord :: String -> Model -> Model
addWord word (voc, i2w, n) = case H.lookup word voc of
  Nothing       -> (H.insert word (n, 1) voc, H.insert n word i2w, n + 1)
  Just (idx, c) -> (H.insert word (idx, c + 1) voc, i2w, n)
