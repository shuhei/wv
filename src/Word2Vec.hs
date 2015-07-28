module Word2Vec ( trainWith
                ) where

import Vocabulary
import Train

import Numeric.LinearAlgebra.Data (loadMatrix, saveMatrix)

trainWith :: FilePath -> IO ()
trainWith path = do
  putStrLn "Reading sentences"
  sentences <- readSentences path

  putStrLn "Training"
  let (voc, i2w, vocSize) = buildModel sentences
      matrices = buildMatrices vocSize
      (i2h, h2o) = train voc matrices sentences

  putStrLn "Saving matrices"
  saveMatrix "i2h.txt" "%g" i2h
  saveMatrix "h2o.txt" "%g" h2o

  return ()

readSentences :: FilePath -> IO [Sentence]
readSentences path = do
  contents <- readFile path
  return $ map words $ lines contents
