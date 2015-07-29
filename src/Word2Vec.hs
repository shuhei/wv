module Word2Vec ( trainWith
                , mostSimilar
                ) where

import Vocabulary
import Huffman
import Train

import Prelude hiding (lookup)
import Control.Monad (liftM)
import Numeric.LinearAlgebra.Data (loadMatrix, saveMatrix)
import Data.HashMap.Strict (HashMap, toList, fromList, empty, lookup)
import Data.Hashable (Hashable)
import System.Random (getStdGen)
import System.IO

i2hFile = "i2h.txt"
i2wFile = "i2w.txt"
w2iFile = "w2i.txt"

-- | Train input-hidden weight matrix with sentences from a file.
trainWith :: FilePath -> IO ()
trainWith path = do
  sentences <- readSentences path

  randGen <- getStdGen
  let (voc, i2w, vocSize) = buildModel sentences
      coding = encodeTree $ buildTree $ map snd $ toList voc
      matrices = buildMatrices randGen vocSize
      (i2h, h2o) = train voc coding matrices sentences

  saveMatrix i2hFile "%g" i2h
  saveI2W i2wFile i2w
  saveW2I w2iFile voc

mostSimilar :: [String] -> IO ()
mostSimilar words = do
  i2h <- loadMatrix i2hFile
  i2w <- loadI2W i2wFile
  w2i <- loadW2I w2iFile
  let word = head words
  case lookup word w2i of
    Just idx -> do
      -- TODO: Find the most similar vector.
      return ()
    Nothing -> putStrLn $ word ++ "is not in the vocabulary."

-- | Read sentences from a normalized file.
readSentences :: FilePath -> IO [Sentence]
readSentences path = do
  contents <- readFile path
  return $ map words $ lines contents

saveI2W :: FilePath -> Index2Word -> IO ()
saveI2W = saveHashMap (\(i, w) -> show i ++ " " ++ w)

saveW2I :: FilePath -> Vocabulary -> IO ()
saveW2I = saveHashMap (\(w, (i, _)) -> w ++ " " ++ show i)

loadI2W :: FilePath -> IO Index2Word
loadI2W = loadHashMap (\(i:w:_) -> (read i, w))

loadW2I :: FilePath -> IO (HashMap String Int)
loadW2I = loadHashMap (\(w:i:_) -> (w, read i))

-- | Load HashMap from a file.
loadHashMap :: (Eq a, Hashable a) => ([String] -> (a, b)) -> FilePath -> IO (HashMap a b)
loadHashMap readPair path = liftM readMap (readFile path)
  where readMap = fromList . map (readPair . words) . lines

-- | Save HashMap to a file.
saveHashMap :: ((a, b) -> String) -> FilePath -> HashMap a b -> IO ()
saveHashMap showPair path = writeFile path . showMap
  where showMap = unlines . map showPair . toList
