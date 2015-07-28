module Main where

import Word2Vec

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  mapM_ putStrLn args
  case args of
    "train" : path : _ -> trainWith path
    _ -> showHelp

showHelp :: IO ()
showHelp = do
  putStrLn "usage: wv <command> [<args>]"
  putStrLn ""
  putStrLn "  train <file path>    Train matrices and save them as files"
  putStrLn ""
