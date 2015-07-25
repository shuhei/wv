module Bigram
    ( countPairs
    , printPairCounts
    , generate
    ) where

import Data.List
import Data.Ord(comparing)
import Data.Monoid

-- | Word pair and its count.
type PairCount = (String, String, Int)

from :: PairCount -> String
from (f, _, _) = f

to :: PairCount -> String
to (_, t, _) = t

count :: PairCount -> Int
count (_, _, c) = c

-- | Express 'PairCount' as a 'String'.
showContent :: PairCount -> String
showContent (f, t, c) = f ++ " -> " ++ t ++ " = " ++ show c

-- | Check if a given pair of words matches a 'PairCount'.
matchPair :: String -> String -> PairCount -> Bool
matchPair f t (ff, tt, _) = f == ff && t == tt

matchFrom :: String -> PairCount -> Bool
matchFrom str (f, _, _) = str == f

-- | Add a pair of words to a 'List' of 'PairCount'.
add :: [PairCount] -> (String, String) -> [PairCount]
add bag (f, t) = case partition (matchPair f t) bag of
  ([], _)                 -> (f, t, 1) : bag
  ([(_, _, count)], rest) -> (f, t, count + 1) : rest

-- | Order 'PairCount' by count, from and to.
cmp :: PairCount -> PairCount -> Ordering
cmp = flip (comparing count) `mappend` (comparing from) `mappend` (comparing to)

-- | Count word pairs in a text.
countPairs :: String -> [PairCount]
countPairs str =
  let ws = words str in
  let wws = zip ws $ tail ws in
  sortBy cmp $ foldl add [] $ wws

-- | Print '[PairCount]' to the console.
printPairCounts :: [PairCount] -> IO ()
printPairCounts counts = mapM_ (putStrLn . showContent) $ take 10 counts

-- | Generate a sentence with a starting word.
generate :: String -> Int -> [PairCount] -> [String]
generate start num counts = start : generate' start num counts

generate' :: String -> Int -> [PairCount] -> [String]
generate' word num counts = case num of
  0 -> []
  _ -> case filter (matchFrom word) counts of
    [] -> [] -- No more candidate.
    (_, t, _) : rest -> t : (generate' t (num - 1) counts)
