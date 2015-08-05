module Persistence ( loadHashMap
                   , saveHashMap
                   , loadIntMap
                   , saveIntMap
                   ) where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Control.Monad (liftM)

-- | Load HashMap from a file.
loadHashMap :: (Eq a, Hashable a) => ([String] -> (a, b)) -> FilePath -> IO (HM.HashMap a b)
loadHashMap readPair path = liftM readMap (readFile path)
  where readMap = HM.fromList . map (readPair . words) . lines

-- | Save HashMap to a file.
saveHashMap :: ((a, b) -> String) -> FilePath -> HM.HashMap a b -> IO ()
saveHashMap showPair path = writeFile path . showMap
  where showMap = unlines . map showPair . HM.toList

-- | Load IntMap from a file.
loadIntMap :: ([String] -> (Int, b)) -> FilePath -> IO (IM.IntMap b)
loadIntMap readPair path = liftM readMap (readFile path)
  where readMap = IM.fromList . map (readPair . words) . lines

-- | Save IntMap to a file.
saveIntMap :: ((Int, b) -> String) -> FilePath -> IM.IntMap b -> IO ()
saveIntMap showPair path = writeFile path . showMap
  where showMap = unlines . map showPair . IM.toList
