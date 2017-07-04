{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = classify (words s)
  where classify ("I":ts:xs) = LogMessage Info (read ts) (unwords xs)
        classify ("W":ts:xs) = LogMessage Warning (read ts) (unwords xs)
        classify ("E":n:ts:xs) = LogMessage (Error (read n)) (read ts) (unwords xs)
        classify l = Unknown (unwords l)


parse :: String -> [LogMessage]
parse file = parseLines (lines file)
  where parseLines [] = []
        parseLines (x:xs) = parseMessage x : parseLines xs


-- insert :: LogMessage -> MessageTree -> MessageTree
