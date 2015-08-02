module Persistence ( loadHashMap
                   , saveHashMap
                   ) where

import Data.HashMap.Strict (HashMap, fromList, toList)
import Data.Hashable (Hashable)
import Control.Monad (liftM)

-- | Load HashMap from a file.
loadHashMap :: (Eq a, Hashable a) => ([String] -> (a, b)) -> FilePath -> IO (HashMap a b)
loadHashMap readPair path = liftM readMap (readFile path)
  where readMap = fromList . map (readPair . words) . lines

-- | Save HashMap to a file.
saveHashMap :: ((a, b) -> String) -> FilePath -> HashMap a b -> IO ()
saveHashMap showPair path = writeFile path . showMap
  where showMap = unlines . map showPair . toList
