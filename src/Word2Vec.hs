module Word2Vec ( trainWith
                , mostSimilar
                ) where

import Vocabulary
import Huffman
import Train
import Persistence

import Prelude hiding (lookup)
import Numeric.LinearAlgebra.Data (Matrix, Vector, loadMatrix, saveMatrix, cmap, size)
import qualified Numeric.LinearAlgebra.HMatrix as M
import qualified Numeric.LinearAlgebra.Devel as DV
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Foldable (fold)
import Control.Arrow (first)
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
      coding = encodeTree $ buildTree $ map snd $ HM.toList voc
      matrices = buildMatrices randGen vocSize
      (i2h, h2o) = train voc coding matrices sentences

  saveMatrix i2hFile "%g" i2h
  saveI2W i2wFile i2w
  saveW2I w2iFile voc

mostSimilar :: [String] -> IO ()
mostSimilar [] = putStrLn "Specify words."
mostSimilar words = do
  i2h <- normalizeMatrix <$> loadMatrix i2hFile
  i2w <- loadI2W i2wFile
  w2i <- loadW2I w2iFile
  mapM_ print $ take 5 $ findMostSimilar i2w w2i i2h words

findMostSimilar :: HM.HashMap Int String
                -> HM.HashMap String Int
                -> Matrix Double
                -> [String]
                -> [(String, Double)]
findMostSimilar i2w w2i i2h words = filter ((`notElem` nakedWords) . fst) similarWords
  where weightedWords = map weightedWord words
        nakedWords = map fst weightedWords
        similarities = M.toList $ M.app i2h (wordsMean w2i i2h weightedWords)
        indexSimilarities = sortBy (flip $ comparing snd) $ zip [0..] similarities
        similarWords = map (first (i2w HM.!)) indexSimilarities

-- | Normalized mean vector for multiple words.
wordsMean :: HM.HashMap String Int -> Matrix Double -> [(String, Double)] -> Vector Double
wordsMean w2i i2h words = meanVector vecs
  where vecs = map (\(s, w) -> M.scale w $ wordVector w2i i2h s) words

-- | Weigh a word according to its prefix.
weightedWord :: String -> (String, Double)
weightedWord word = case word of
  '+':rest -> (rest, 1)
  '-':rest -> (rest, -1)
  word     -> (word, 1)

-- | Return a vector for a word.
wordVector :: HM.HashMap String Int -> Matrix Double -> String -> Vector Double
wordVector w2i i2h = (i2h M.!) . (w2i HM.!)

-- | Normalized mean vector.
meanVector :: [Vector Double] -> Vector Double
meanVector vecs = M.scale (1 / magnitude' mean) mean
  where mean = sum vecs / fromIntegral (length vecs)

-- | Divide matrix values by each row vector's magnitude.
normalizeMatrix :: Matrix Double -> Matrix Double
normalizeMatrix mat = DV.runSTMatrix $ do
  m <- DV.thawMatrix mat
  mapM_ (uncurry $ modifyRow m) $ zip [0..] magnitudes
  return m
    where magnitudes = map magnitude' $ M.toRows mat
          colSize = M.cols mat
          modifyRow mm i mag = mapM_ (flip (DV.modifyMatrix mm i) (/ mag)) [0 .. colSize - 1]

-- | Calculate magnitude of a 'Vector'.
magnitude :: Vector Double -> Double
magnitude = sqrt . M.sumElements . (** 2)

-- | Calculate magnitude of a 'Vector'. Return 1 if magnitude is 0 not to
-- divide by 0.
magnitude' :: Vector Double -> Double
magnitude' v = if mag == 0 then 1 else mag
  where mag = magnitude v

-- | Read sentences from a normalized file.
readSentences :: FilePath -> IO [Sentence]
readSentences path = do
  contents <- readFile path
  {- return $ map words $ lines contents -}
  return $ take 1000000 $ map words $ lines contents

saveI2W :: FilePath -> Index2Word -> IO ()
saveI2W = saveHashMap (\(i, w) -> show i ++ " " ++ w)

saveW2I :: FilePath -> Vocabulary -> IO ()
saveW2I = saveHashMap (\(w, (i, _)) -> w ++ " " ++ show i)

loadI2W :: FilePath -> IO Index2Word
loadI2W = loadHashMap (\(i:w:_) -> (read i, w))

loadW2I :: FilePath -> IO (HM.HashMap String Int)
loadW2I = loadHashMap (\(w:i:_) -> (w, read i))
