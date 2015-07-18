module Main where

import Bigram
import System.Environment

main :: IO ()
main = do
  counts <- fmap countPairs getContents
  printPairCounts counts
  putStrLn "------------"
  args <- getArgs
  case args of
    [] -> putStrLn "Specify starting word."
    start : _ -> mapM_ putStrLn $ generate start 10 counts
