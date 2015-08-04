module Main where

import Word2Vec

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "train" : path : num : _ -> trainWith path $ read num
    "most-similar" : words@(w:ws) -> mostSimilar words
    _ -> showHelp

showHelp :: IO ()
showHelp = do
  putStrLn "usage: wv <command> [<args>]"
  putStrLn ""
  putStrLn "  train <file path>      Train matrices and save them as files"
  putStrLn "  most-similar <words>   List most similar words to the given ones"
  putStrLn ""
